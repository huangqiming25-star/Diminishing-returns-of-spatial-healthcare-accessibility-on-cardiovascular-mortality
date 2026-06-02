import os
import numpy as np
import geopandas as gpd
import rasterio
from rasterio.warp import reproject, Resampling
import pandas as pd
from rasterio import features

country_shp_path = r':\World Bank Official Boundaries - Admin 0\WB_GAD_ADM0.shp'
density_dir = r'D:\heathycare data\data and code\TEST\FIG1\healthycare_density'
pop_tif_dir = r'D:\heathycare data\data and code\TEST\FIG1\healthycare density'
output_raster_dir = r':\heathycare data\data and code\TEST\FIG1'
output_csv_path = r':\heathycare data\data and code\TEST\FIG1\pop_weighted_density.csv'

os.makedirs(output_raster_dir, exist_ok=True)
os.makedirs(os.path.dirname(output_csv_path), exist_ok=True)

print("Reading country boundary shapefile...")
country_shp = gpd.read_file(country_shp_path)
print(f"Number of countries: {len(country_shp)}")

sample_tif_path = os.path.join(density_dir, 'density_all_2014.tif')
with rasterio.open(sample_tif_path) as src:
    ref_transform = src.transform
    ref_crs = src.crs
    nx, ny = src.width, src.height

print(f"Reference raster size: {nx} x {ny}")

print("Rasterizing country boundaries...")
country_raster = np.zeros((ny, nx), dtype=np.int32) - 1

for idx, country in country_shp.iterrows():
    geom = country.geometry
    country_id = idx
    mask = features.rasterize(
        [(geom, country_id)],
        out_shape=(ny, nx),
        transform=ref_transform,
        fill=-1,
        dtype=np.int32
    )
    country_raster = np.where(mask != -1, mask, country_raster)

years = range(2014, 2025)
categories = ['hospital', 'clinic', 'pharmacy', 'all']

results = []
total_tasks = len(years) * len(categories)
task_count = 0
nodata_val = -9999

for year in years:
    print(f"\nProcessing year: {year}")
    for cate in categories:
        task_count += 1
        print(f"[{task_count}/{total_tasks}] Processing category: {cate}")

        density_tif_path = os.path.join(density_dir, f'density_{cate}_{year}.tif')
        pop_tif_path = os.path.join(pop_tif_dir, f'landscan-global-{year}.tif')
        out_tif_path = os.path.join(output_raster_dir, f'pop_weighted_{cate}_{year}.tif')

        if not os.path.exists(density_tif_path):
            print(f" - Warning: Density raster file not found, skipping: {density_tif_path}")
            for country_idx, country in country_shp.iterrows():
                results.append({
                    'NAM_0': country['NAM_0'],
                    'pop_weighted_density': np.nan,
                    'total_pop': 0,
                    'year': year,
                    'category': cate
                })
            continue

        if not os.path.exists(pop_tif_path):
            print(f" - Warning: Population raster file not found, skipping: {pop_tif_path}")
            for country_idx, country in country_shp.iterrows():
                results.append({
                    'NAM_0': country['NAM_0'],
                    'pop_weighted_density': np.nan,
                    'total_pop': 0,
                    'year': year,
                    'category': cate
                })
            continue

        with rasterio.open(density_tif_path) as src:
            density_arr = src.read(1).astype(np.float32)
            profile = src.profile

        density_arr = np.where(density_arr == -999, 0, density_arr)
        print(f" - Reading density raster: {density_tif_path}")

        with rasterio.open(pop_tif_path) as src:
            pop_arr = src.read(1).astype(np.float32)
            pop_transform = src.transform
            pop_crs = src.crs

        pop_arr[pop_arr < 0] = 0
        original_pop_total = np.nansum(pop_arr)
        print(f" - Reading population raster: {pop_tif_path}, total population: {original_pop_total:.0f}")

        pop_resampled = np.empty_like(density_arr, dtype=np.float32)
        reproject(
            source=pop_arr,
            destination=pop_resampled,
            src_transform=pop_transform,
            src_crs=pop_crs,
            dst_transform=ref_transform,
            dst_crs=ref_crs,
            resampling=Resampling.bilinear
        )
        pop_resampled[pop_resampled < 0] = 0

        resampled_pop_total = np.nansum(pop_resampled)
        pop_ratio = resampled_pop_total / original_pop_total if original_pop_total > 0 else 1
        print(f" - Population resampling completed, total change ratio: {pop_ratio:.4f}")

        weighted_arr = density_arr * pop_resampled
        weighted_arr_out = np.where(np.isnan(weighted_arr), nodata_val, weighted_arr)
        profile.update(dtype=rasterio.float32, nodata=nodata_val)

        with rasterio.open(out_tif_path, 'w', **profile) as dst:
            dst.write(weighted_arr_out.astype(np.float32), 1)
        print(f" - Population-weighted raster output completed: {out_tif_path}")

        country_stats = []

        for country_idx, country in country_shp.iterrows():
            country_mask = country_raster == country_idx

            total_country_pop = np.sum(pop_resampled[country_mask & (~np.isnan(pop_resampled))])

            valid_mask = country_mask & (~np.isnan(density_arr)) & (~np.isnan(pop_resampled)) & (pop_resampled > 0)

            if np.any(valid_mask):
                valid_density = density_arr[valid_mask]
                valid_pop = pop_resampled[valid_mask]
                numerator = np.sum(valid_density * valid_pop)
            else:
                numerator = 0

            if total_country_pop > 0:
                weighted_density = numerator / total_country_pop
            else:
                weighted_density = 0

            country_stats.append({
                'NAM_0': country['NAM_0'],
                'pop_weighted_density': weighted_density,
                'total_pop': total_country_pop,
                'year': year,
                'category': cate
            })

        results.extend(country_stats)
        print(f" - Completed statistics for {len(country_stats)} countries")

if results:
    df_all = pd.DataFrame(results)

    final_columns = ['year', 'category', 'NAM_0', 'pop_weighted_density', 'total_pop']
    df_all = df_all[final_columns]

    df_all = df_all.sort_values(['year', 'category', 'NAM_0'])

    df_all['pop_weighted_density'] = pd.to_numeric(df_all['pop_weighted_density'], errors='coerce')
    df_all['total_pop'] = pd.to_numeric(df_all['total_pop'], errors='coerce')

    df_all.to_csv(output_csv_path, index=False, encoding='utf-8-sig')
    print(f"\nCompleted! Population-weighted healthcare infrastructure CSV output to: {output_csv_path}")
    print(f"Total records: {len(df_all)}")
    print(f"Number of countries: {df_all['NAM_0'].nunique()}")
    print(f"Year range: {df_all['year'].min()} - {df_all['year'].max()}")

    valid_density = df_all['pop_weighted_density'].dropna()
    if len(valid_density) > 0:
        print(f"Population-weighted density statistics:")
        print(f"  Min: {valid_density.min():.6f}")
        print(f"  Max: {valid_density.max():.6f}")
        print(f"  Mean: {valid_density.mean():.6f}")
        print(f"  Median: {valid_density.median():.6f}")
else:
    print("\nWarning: No data processed, possibly due to incorrect file paths or missing files")