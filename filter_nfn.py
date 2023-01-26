import pandas as pd
import numpy as np
from collections import defaultdict
import statistics


input_csv = input('Input Path to CSV (Go to the folder where your CSV file is, then click "Copy as Path" if using Windows)')
try:
    csv_to_be_filtered = pd.read_csv(input_csv)
except FileNotFoundError:
    raise FileNotFoundError("This File Doesn't Exist! You probably put in the wrong Path.")


tegula_x1 = csv_to_be_filtered['T2_1 Tegula length: x1']
tegula_x1 = np.array(tegula_x1)
tegula_x2 = csv_to_be_filtered['T2_1 Tegula length: x2']
tegula_x2 = np.array(tegula_x2)
tegula_y1 = csv_to_be_filtered['T2_1 Tegula length: y1']
tegula_y1 = np.array(tegula_y1)
tegula_y2 = csv_to_be_filtered['T2_1 Tegula length: y2']
tegula_y2 = np.array(tegula_y2)

scale_x1 = csv_to_be_filtered['T2_1 Set scale (0.5 cm): x1']
scale_x1 = np.array(scale_x1)
scale_x2 = csv_to_be_filtered['T2_1 Set scale (0.5 cm): x2']
scale_x2 = np.array(scale_x2)
scale_y1 = csv_to_be_filtered['T2_1 Set scale (0.5 cm): y1']
scale_y1 = np.array(scale_y1)
scale_y2 = csv_to_be_filtered['T2_1 Set scale (0.5 cm): y2']
scale_y2 = np.array(scale_y2)


tegula_pixel_distance = ( (tegula_x2 - tegula_x1)**2 + (tegula_y2 - tegula_y1)**2 )**0.5 #distance formula

scale_pixel_distance = ( (scale_x2 - scale_x1)**2 + (scale_y2 - scale_y1)**2 )**0.5

pixel_to_cm_conversion = 0.5 / scale_pixel_distance

tegula_distance_cm_array = tegula_pixel_distance * pixel_to_cm_conversion


subject_id_list = csv_to_be_filtered['subject_id']
classification_id_list = csv_to_be_filtered['classification_id']

community_scientist_measurements_grouped_by_image = defaultdict(defaultdict)

for subject_id, classification_id, tegula_distance_cm in zip(subject_id_list, classification_id_list, tegula_distance_cm_array):
    try:
        community_scientist_measurements_grouped_by_image[ subject_id ]['measurement_list'].append(tegula_distance_cm)
    except:
        community_scientist_measurements_grouped_by_image[ subject_id ]['measurement_list'] = []
        community_scientist_measurements_grouped_by_image[ subject_id ]['measurement_list'].append(tegula_distance_cm)
    try:
        community_scientist_measurements_grouped_by_image[ subject_id ]['classification_ids'].append(classification_id)
    except:
        community_scientist_measurements_grouped_by_image[ subject_id ]['classification_ids'] = []
        community_scientist_measurements_grouped_by_image[ subject_id ]['classification_ids'].append(classification_id)


cleaned_community_scientist_measurements_grouped_by_image = defaultdict(list)
removed_measurement_list = []

for subject_id,dictionary_value_lists in community_scientist_measurements_grouped_by_image.items():
    measurement_list = dictionary_value_lists['measurement_list']
    classification_ids = dictionary_value_lists['classification_ids']

    sorted_measurement_list, sorted_classification_ids = [x for x,y in sorted(zip(measurement_list,classification_ids))],[y for x,y in sorted(zip(measurement_list,classification_ids))]

    median = statistics.median(measurement_list)
    upper_bound = median * 1.2 
    lower_bound = median * 0.8

    #this only allows measurements within 20% of the median

    for measurement,classification_id in zip(sorted_measurement_list,sorted_classification_ids):
        if measurement > lower_bound and measurement < upper_bound:
            cleaned_community_scientist_measurements_grouped_by_image[subject_id].append(measurement)
        else:
            removed_measurement_list.append(classification_id)


list_of_csv_line_to_be_removed = []

for removed_measurement in removed_measurement_list:
    csv_line_to_be_removed = list(csv_to_be_filtered['classification_id']).index(removed_measurement)
    list_of_csv_line_to_be_removed.append(csv_line_to_be_removed)

csv_to_be_filtered = csv_to_be_filtered.drop(index = list_of_csv_line_to_be_removed, axis = 0)

csv_to_be_filtered.to_csv(input_csv[:-4]+'-filtered'+'.csv')