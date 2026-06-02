import os
import logging
import pandas as pd
import geopandas as gpd
from shapely.geometry import Point
from shapely import wkb
import osmium
import shutil
import glob
import random

logging.basicConfig(level=logging.INFO, format="%(asctime)s - %(levelname)s - %(message)s")
logger = logging.getLogger(__name__)


class HealthcareHandler(osmium.SimpleHandler):
    def __init__(self, tags_of_interest, geom_type='point', batch_size=10000, temp_dir=None, region_name='tmp'):
        super().__init__()
        self.tags_of_interest = tags_of_interest
        self.geom_type = geom_type
        self.wkbfab = osmium.geom.WKBFactory()
        self.batch = []
        self.batch_size = batch_size
        self.temp_dir = temp_dir
        self.region_name = region_name
        self.file_count = 0
        self.crs = "EPSG:4326"

    def save_batch(self):
        if not self.batch:
            return
        gdf = gpd.GeoDataFrame(self.batch, geometry='geometry', crs=self.crs)
        if not gdf.empty:
            out_path = os.path.join(
                self.temp_dir,
                f"{self.region_name}_{self.geom_type}_{self.file_count}.gpkg"
            )
            logger.info(f"Saving {self.geom_type} batch {self.file_count}, count {len(gdf)}")
            gdf.to_file(out_path, layer='data', driver="GPKG")
            self.file_count += 1
        self.batch.clear()

    def node(self, n):
        if self.geom_type == 'point' and n.location.valid():
            for key, values in self.tags_of_interest.items():
                if key in n.tags and n.tags[key] in values:
                    self.batch.append({
                        'osm_id': n.id,
                        'asset': n.tags[key],
                        'geometry': Point(n.location.lon, n.location.lat)
                    })
                    if len(self.batch) >= self.batch_size:
                        self.save_batch()

    def area(self, a):
        if self.geom_type == 'polygon':
            for key, values in self.tags_of_interest.items():
                if key in a.tags and a.tags[key] in values:
                    try:
                        geom = wkb.loads(self.wkbfab.create_multipolygon(a), hex=True)
                        self.batch.append({
                            'osm_id': a.id,
                            'asset': a.tags[key],
                            'geometry': geom
                        })
                        if len(self.batch) >= self.batch_size:
                            self.save_batch()
                    except Exception:
                        continue

    def close(self):
        self.save_batch()


def merge_temp_gpkg(temp_dir, region_name, geom_type):
    files = glob.glob(os.path.join(temp_dir, f"{region_name}_{geom_type}_*.gpkg"))
    if not files:
        return gpd.GeoDataFrame(columns=['osm_id', 'asset', 'geometry'], geometry='geometry', crs='EPSG:4326')

    gdf_list = [gpd.read_file(f, layer='data') for f in files]
    for f in files:
        os.remove(f)

    merged = pd.concat(gdf_list, ignore_index=True)
    logger.info(f"{geom_type} merged count: {len(merged)}")
    return merged


def compare_polygon_to_point_and_merge(df_point, df_polygon):
    if df_point.empty:
        df_polygon['geometry'] = df_polygon.geometry.centroid
        return df_polygon.reset_index(drop=True)

    if df_polygon.empty:
        return df_point.reset_index(drop=True)

    sindex = df_point.sindex
    keep_idx = []

    for idx, poly in df_polygon.iterrows():
        possible_idx = list(sindex.intersection(poly.geometry.bounds))
        pts = df_point.iloc[possible_idx]
        pts_in = pts[pts.geometry.within(poly.geometry)]
        if pts_in[pts_in['asset'] == poly['asset']].empty:
            keep_idx.append(idx)

    df_polygon = df_polygon.loc[keep_idx].copy()
    df_polygon['geometry'] = df_polygon.geometry.centroid

    merged = pd.concat([df_point, df_polygon], ignore_index=True)
    return gpd.GeoDataFrame(merged, crs=df_point.crs)


def extract_healthcare_from_osm(osm_path, output_dir, region_name="healthcare", batch_size=10000):
    tags = {
        'amenity': ['hospital', 'doctors', 'clinic', 'dentist', 'pharmacy'],
        'healthcare': [
            'doctor', 'pharmacy', 'hospital', 'clinic', 'dentist',
            'physiotherapist', 'alternative', 'laboratory',
            'optometrist', 'rehabilitation', 'blood_donation', 'birthing_center'
        ]
    }

    temp_dir = os.path.join(output_dir, "tmp")
    os.makedirs(temp_dir, exist_ok=True)

    logger.info("Extracting point features...")
    h_point = HealthcareHandler(tags, 'point', batch_size, temp_dir, region_name)
    h_point.apply_file(osm_path, locations=True)
    h_point.close()

    logger.info("Extracting polygon features...")
    h_poly = HealthcareHandler(tags, 'polygon', batch_size, temp_dir, region_name)
    h_poly.apply_file(osm_path, locations=True)
    h_poly.close()

    gdf_point = merge_temp_gpkg(temp_dir, region_name, 'point')
    gdf_poly = merge_temp_gpkg(temp_dir, region_name, 'polygon')

    gdf = compare_polygon_to_point_and_merge(gdf_point, gdf_poly)
    if gdf.empty:
        logger.warning("No healthcare features extracted")
        return

    os.makedirs(output_dir, exist_ok=True)
    out_path = os.path.join(output_dir, f"{region_name}.shp")

    logger.info(f"Writing output: {out_path}")
    gdf.to_file(out_path, driver="ESRI Shapefile")

    shutil.rmtree(temp_dir, ignore_errors=True)


if __name__ == "__main__":
    year = 22
    filename = f"africa-{year}0101.osm.pbf"

    osm_path = os.path.join(r"D:\BaiduNetdiskDownload\extr_health\africa", filename)
    output_dir = os.path.join(r"D:\BaiduNetdiskDownload\healthcare_output\africa", "shp")

    logger.info(f"Processing: {filename}")

    if os.path.exists(osm_path):
        extract_healthcare_from_osm(osm_path, output_dir, f"africa_healthcare_{year}")
    else:
        logger.warning(f"File not found: {osm_path}")