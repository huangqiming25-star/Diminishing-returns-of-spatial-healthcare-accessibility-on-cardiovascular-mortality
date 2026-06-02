import os
import geopandas as gpd
import rasterio
from rasterio.mask import mask
from rasterio.warp import reproject, Resampling
import numpy as np
from shapely.geometry import mapping
from tqdm import tqdm
from concurrent.futures import ProcessPoolExecutor, as_completed

city_shp_path = r"D:\heathycare data\data and code\TEST\FIG1\geoBoundariesCGAZ_ADM2\geoBoundariesCGAZ_ADM2.shp"
density_folder = r"D:\heathycare data\data and code\TEST\FIG1\geoBoundariesCGAZ_ADM2\healthycare density"
pop_folder = r"D:\heathycare data\data and code\TEST\FIG1\geoBoundariesCGAZ_ADM2\healthycare density"
output_shp = r"D:\heathycare data\data and code\TEST\FIG1\geoBoundariesCGAZ_ADM2\op_weighted_density.shp"
years = range(2014, 2025)

cities = gpd.read_file(city_shp_path).to_crs(epsg=4326)
print(f"✅ Loaded {len(cities)} cities")

def resample_pop_to_density(pop_path, density_path):
    with rasterio.open(density_path) as dens_src, rasterio.open(pop_path) as pop_src:
        pop_data = pop_src.read(1)
        pop_data_resampled = np.empty((dens_src.height, dens_src.width), dtype=np.float32)

        reproject(
            source=pop_data,
            destination=pop_data_resampled,
            src_transform=pop_src.transform,
            src_crs=pop_src.crs,
            dst_transform=dens_src.transform,
            dst_crs=dens_src.crs,
            resampling=Resampling.bilinear
        )

        dens_data = dens_src.read(1)
        dens_meta = dens_src.meta.copy()

    return pop_data_resampled, dens_data, dens_meta

for year in years:
    cities[f"density_{year}"] = np.nan

total_skipped = 0

for year in tqdm(years, desc="Processing years"):
    dens_path = os.path.join(density_folder, f"density_all_{year}.tif")
    pop_path = os.path.join(pop_folder, f"landscan-global-{year}.tif")

    if not (os.path.exists(dens_path) and os.path.exists(pop_path)):
        print(f"⚠️ Missing files: {year}")
        continue

    pop_resampled, dens_data, dens_meta = resample_pop_to_density(pop_path, dens_path)

    dens_data = np.where(dens_data > 0, dens_data, np.nan)
    pop_resampled = np.where(pop_resampled > 0, pop_resampled, np.nan)

    dens_meta.update(dtype='float32', count=1)
    with rasterio.MemoryFile() as mem_dens, rasterio.MemoryFile() as mem_pop:
        with mem_dens.open(**dens_meta) as dens_tmp, mem_pop.open(**dens_meta) as pop_tmp:
            dens_tmp.write(dens_data, 1)
            pop_tmp.write(pop_resampled, 1)

            for idx, city in cities.iterrows():
                geom = [mapping(city.geometry)]
                try:
                    dens_crop, _ = mask(dens_tmp, geom, crop=True)
                    pop_crop, _ = mask(pop_tmp, geom, crop=True)

                    dens_values = dens_crop[0]
                    pop_values = pop_crop[0]

                    valid_mask = (~np.isnan(dens_values)) & (~np.isnan(pop_values)) & (pop_values > 0)
                    if np.sum(valid_mask) == 0:
                        weighted_density = np.nan
                    else:
                        numerator = np.nansum(dens_values[valid_mask] * pop_values[valid_mask])
                        denominator = np.nansum(pop_values[valid_mask])
                        weighted_density = numerator / denominator if denominator > 0 else np.nan

                    cities.at[idx, f"density_{year}"] = weighted_density

                except Exception as e:
                    total_skipped += 1
                    city_name = city.get("NAME_2", f"city_{idx}")
                    print(f"⚠️ City {city_name} error: {e}")
                    continue

density_cols = [f"density_{year}" for year in years]
cities['density_avg'] = cities[density_cols].mean(axis=1, skipna=True)

cities.to_file(output_shp, encoding='utf-8')

print(f"\n✅ Done! SHP saved to: {output_shp}")
print(f"⚠️ Total skipped cities: {total_skipped} (no overlap with raster or error)")