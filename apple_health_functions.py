# Code mostly stolen from https://medium.com/internet-of-technology/how-to-extract-and-analyze-apple-health-data-a-practical-python-guide-4b9ddbade37f


import xml.etree.ElementTree as ET
import pandas as pd
import datetime as dt

def calorie_summary(cal_goal):
    #cal_goal = 17500

    # Load and parse the XML file.
    # (Optional): Load and parse the export_cda.xml file.
    tree = ET.parse('export.xml')
    root = tree.getroot()

    # Create a set to store unique @type values.
    types_set = set()

    # Iterate through the XML elements and extract @type attribute.
    #for record in root.findall('.//Record'):
    #    type_attribute = record.get('type')
    #    if type_attribute:
    #        types_set.add(type_attribute)

    # Print all unique @type values.
    #for type_value in types_set:
    #    print(type_value)

    records = []

    for record in root.findall(".//Record[@type='HKQuantityTypeIdentifierBasalEnergyBurned']"):
        records.append({
            'start_date': record.get('startDate'),
            'end_date': record.get('endDate'),
            'value': record.get('value'),
            'unit': record.get('unit'),
        })

    # Convert to pandas df
    basal_energy = pd.DataFrame(records)

    # Convert start date to date
    basal_energy['start_date'] = pd.to_datetime(basal_energy['start_date'])

    # Convert to eastern?
    basal_energy['est_start'] = basal_energy['start_date'].dt.tz_convert('US/Eastern')

    # Find max date
    # Extract just the date part of the max timestamp
    max_date = basal_energy['est_start'].max()

    # Extract just the date part of the max timestamp
    max_date = max_date.date()

    # Keep data starting May 22nd, 2025 [day after I got my watch]
    f_basal = basal_energy[basal_energy['est_start'] > '2025-05-22']

    # Remove partial data using the max date
    f_basal = f_basal[f_basal['est_start'].dt.date != max_date]

    # Convert timestamp to date
    f_basal['est_start_date'] = f_basal['est_start'].dt.strftime('%y-%m-%d')

    # Convert cals from string to numeric
    f_basal.value = pd.to_numeric(f_basal.value, errors='coerce')

    # Group basal energy by day using start date
    f_basal =\
        f_basal\
        .groupby(['est_start_date'])\
        .agg({'value':'sum'})

    f_basal.reset_index(inplace=True)
    f_basal.rename(columns={'value':'basal_energy'}, inplace=True)
    f_basal

    # Repeat for active energy so we can get total calories by day
    records = []

    for record in root.findall(".//Record[@type='HKQuantityTypeIdentifierActiveEnergyBurned']"):
        records.append({
            'start_date': record.get('startDate'),
            'end_date': record.get('endDate'),
            'value': record.get('value'),
            'unit': record.get('unit'),
        })

    # Convert to pandas df
    active_energy = pd.DataFrame(records)

    # Convert start date to date
    active_energy['start_date'] = pd.to_datetime(active_energy['start_date'])

    # Convert to eastern?
    active_energy['est_start'] = active_energy['start_date'].dt.tz_convert('US/Eastern')

    # Keep data starting May 22nd, 2025 [day after I got my watch]
    f_active = active_energy[active_energy['est_start'] > '2025-05-22']

    # Remove partial data using the max date
    f_active = f_active[f_active['est_start'].dt.date != max_date]

    # Convert timestamp to date
    f_active['est_start_date'] = f_active['est_start'].dt.strftime('%y-%m-%d')

    # Convert cals from string to numeric
    f_active.value = pd.to_numeric(f_active.value, errors='coerce')

    # Group active energy by day using start date
    f_active =\
        f_active\
        .groupby(['est_start_date'])\
        .agg({'value':'sum'})

    f_active.reset_index(inplace=True)
    f_active.rename(columns={'value':'active_energy'}, inplace=True)
    f_active

    # Merge the active energy to the basal energy
    f_health =\
        f_basal\
        .merge(
            right = f_active,
            how = "left",
            left_on = ['est_start_date'],
            right_on = ['est_start_date'])

    # Calculations
    f_health['total_cals'] = f_health['basal_energy'] + f_health['active_energy']

    # Convert start date back to a date
    f_health['est_start_date'] = pd.to_datetime(f_health['est_start_date'], format='%y-%m-%d')

    # Calculate week start Monday
    f_health['week_start_mon'] = f_health['est_start_date'].dt.to_period('W').apply(lambda r: r.start_time)

    # Get total calories by week
    f_health_wk =\
        f_health\
        .groupby(['week_start_mon'])\
        .agg({'total_cals':'sum',
            'est_start_date':'count'})

    f_health_wk.reset_index(inplace=True)

    # avg burn per day so far
    f_health_wk['avg_burn'] = f_health_wk['total_cals'] / f_health_wk['est_start_date']

    # Distance to goal
    f_health_wk['cals_to_burn'] = cal_goal - f_health_wk['total_cals']

    # Rest of week burn pace
    f_health_wk['burn_pace_for_goal'] = f_health_wk['cals_to_burn'] / (7 - f_health_wk['est_start_date'])

    # Filter the f_health table down to just the current week
    f_health_now = f_health[f_health['week_start_mon'] == f_health['week_start_mon'].max()]

    # Filter the f_health_weekly table down to just the current week
    f_health_now_wk = f_health_wk[f_health_wk['week_start_mon'] == f_health_wk['week_start_mon'].max()]

    # Summary variables
    through_date = f_health_now['est_start_date'].max()
    total_burn = round(f_health_now_wk.loc[1, 'total_cals'],2)
    to_burn = f_health_now_wk.loc[1, 'cals_to_burn']
    avg_goal = round(f_health_now_wk.loc[1, 'burn_pace_for_goal'],2)
    days_remaining = 7 - f_health_now_wk.loc[1, 'est_start_date']

    # Print a long concatenated summary
    summary = f"""

    So far this week starting Monday through {through_date} 
    you have burned {total_burn} calories. 
    To reach a total of {cal_goal} calories burned this week you need to burn
    an average of {avg_goal} calories for the next {days_remaining} days

    Here is an overview of your calories burned so far this week
    """
    return print(summary), print(f_health_now)



