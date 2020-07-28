import pandas as pd
import numpy as np
from scipy.stats import linregress

pd.set_option('display.max_rows', 75)
pd.set_option('display.max_columns', 25)
pd.set_option('display.width', 1000)

mergedir = pd.read_csv("/Users/peterhoover/Documents/Analysis_Projects/R_Scripts/Self_Reports",parse_dates=True)

#mergedir = r"\\WRNMDFPISISMBD1\DeptShares$\Dept6\NICoE_Informatics\NICoE_Informatics\BHDP\ALL_DATA\Mergeing"

#bhdp_file = pd.read_csv("Users/peterhoover/Documents/Analysis_Projects/R Scripts/Self_Reports\DEID_BHDP_TBI_Surveys.csv"

### Read in BHDP data at the mergedir location. Filters to only the NSI surveys, and merges in other surveys that took
### place at that same date, and location for that person
### output: a data frame of all NSI surveys and the corresponding surveys taken on that edi, date, and location

def read_bhdp(mergedir):
    df = pd.read_csv(mergedir + "\\DEID_BHDP_TBI_Surveys.csv", parse_dates=True)
    cols = list(df.columns)
    surveylist = ["NSI:", "HIT:", "PHQ9", "AUDIT", "PSQI", "ISI", "GAD", "PCL", "QOL"]
    i = 1
    for survey in surveylist:
        print("On survey " + str(i) + " out of " + str(len(surveylist)))
        surveycols = [col for col in cols if survey in col]
        collist = surveycols + ['SubjectID', "start_date", "SurveyName", "parent_dmis_key_name", "child_dmis_key_name",
                                "child_dmis_key_code", 'end_date']
        subdf = df[collist]
        subdf = subdf.dropna(how='all', subset=surveycols)
        if survey == "NSI:":
            edilist = subdf
        else:
            edilist = pd.merge(edilist, subdf, how='left',
                               on=['SubjectID', "start_date", "end_date", "parent_dmis_key_name",
                                   "child_dmis_key_name", "child_dmis_key_code"])
        i += 1
        print(len(edilist))
    print(edilist)
    NSIcols = [col for col in cols if "NSI:" in col]
    nsi = edilist.dropna(how="all", subset=NSIcols).rename(columns={'child_dmis_key_code': 'DMIS ID'})
    #dmiszips = pd.read_csv(mergedir + r"\dmis_to_zips.csv")
    #dmiszips = dmiszips[['DMIS ID', 'Facility 5-Digit ZIP Code', "Facility FIPS Country Code", "Facility State Code",
    #                     "Facility City Name"]]
    #nsi = pd.merge(nsi, dmiszips, how='left', on='DMIS ID')
    return nsi


### scoring for the GAD, HIT6, NSI Total, PHQ9, ISI, and PCL5
### output: a data frame that includes total score columns for the GAD, HIT6, NSI Total, PHQ9, ISI, and PCL5
def scoring_1(nsi, cols):
    nsi["GAD_Score"] = nsi[['GAD 7:Q3_V1', 'GAD 7:Q4_V1', 'GAD 7:Q5_V1', 'GAD 7:Q6_V1',
                            'GAD 7:Q7_V1', 'GAD 2:Q1_V1', 'GAD 2:Q2_V1']].apply(pd.to_numeric, errors='coerce').replace(
        888, np.nan).sum(axis=1, skipna=False)

    nsi["HIT6_Score"] = nsi[['HIT:Irritated_V1', 'HIT:Limit_Activities_V1', 'HIT:Pain_Severity_V1', 'HIT:Too_Tired_V1',
                             'HIT:Trouble_Concentrate_V1', 'HIT:Wish_to_Lie_Down_V1']].apply(pd.to_numeric,
                                                                                             errors='coerce').replace(
        888, np.nan).sum(axis=1, skipna=False)

    nsi['NSI_TotalScore'] = nsi[
        ['NSI:Anxious_V1', 'NSI:Appetite_Change_V1', 'NSI:Balance_V1', 'NSI:Clumsy_V1', 'NSI:Decision_Difficulty_V1',
         'NSI:Depressed_V1', 'NSI:Distracted_V1', 'NSI:Dizzy_V1', 'NSI:Fatigue_V1', 'NSI:Forgetfulness_V1',
         'NSI:Headaches_V1', 'NSI:Hearing_Difficulty', 'NSI:Irritability_V1', 'NSI:Light_Sensitivity_V1',
         'NSI:Nausea_V1', 'NSI:Noise_Sensitivity_V1', 'NSI:Numbness_V1', 'NSI:Overwhelmed_V1',
         'NSI:Sleep_Difficulty_V1', 'NSI:Slow_Thinking_V1', 'NSI:Taste_Smell_Change_V1',
         'NSI:Vision_Problems_V1']].apply(pd.to_numeric, errors='coerce').replace(888, np.nan).sum(axis=1, skipna=False)

    nsi['PHQ9_Score'] = nsi[
        ['PHQ9.APPETITE_V1', 'PHQ9.DEPRESSED_V1', 'PHQ9.FEEL_BAD_V1', 'PHQ9.FEEL_TIRED_V1', 'PHQ9.MOVE_SLOWLY_V1',
         'PHQ9.NO_INTEREST_V1',
         'PHQ9.TROUBLE_CONCENTRATING_V1', 'PHQ9.TROUBLE_SLEEPING_V1', 'PHQ9:Q9_V1']].apply(pd.to_numeric,
                                                                                           errors='coerce').replace(888,
                                                                                                                    np.nan).sum(
        axis=1, skipna=False)

    nsi['AUDIT_Score'] = nsi[
        ['AUDIT-C.HOW_MANY_DRINKS_V1', 'AUDIT-C.HOW_OFTEN_DRINK_V1', 'AUDIT-C.SIX_OR_MORE_DRINK_V1', 'AUDIT:Q10_V1',
         'AUDIT:Q4_V1', 'AUDIT:Q5_V1',
         'AUDIT:Q6_V1', 'AUDIT:Q7_V1', 'AUDIT:Q8_V1', 'AUDIT:Q9_V1']].apply(pd.to_numeric, errors='coerce').replace(888,
                                                                                                                    np.nan).sum(
        axis=1, skipna=False)

    nsi['ISI_Score'] = nsi[[x for x in cols if "ISI" in x]].apply(pd.to_numeric, errors='coerce').replace(888,
                                                                                                          np.nan).sum(
        axis=1, skipna=False)

    nsi['pcl520_tot_score'] = nsi[[x for x in cols if "PCL" in x]].apply(pd.to_numeric, errors='coerce').replace(888,
                                                                                                                 np.nan).sum(
        axis=1, skipna=False)
    return nsi


### scoring for the WHOQOL
### output: a data frame that includes total score column for the WHOQOL
###WHOQOL Scoring
def whoqol_scoring(nsi):
    recode = {1: 5, 2: 4, 3: 3, 4: 2, 5: 1}

    qolcols = ['QOL:Pain_Limitations_V1', 'QOL:Medical_Treatment_V1', 'QOL:Negative_Feelings_Frequency_V1']
    for col in qolcols:
        nsi[col].map(recode)

    d1 = nsi[['QOL:Pain_Limitations_V1', 'QOL:Medical_Treatment_V1', 'QOLL:Energy_Level_V1', 'QOL:Mobility_V1',
              'QOL:Sleep_Satisfaction_V1', 'QOL:Work_Capacity_Satisfaction_V1',
              'QOL:Daily_Activities_Satisfaction_V1']].apply(pd.to_numeric, errors='coerce').replace(888, np.nan).sum(
        axis=1, skipna=False)
    d2 = nsi[
        ['QOL:Life_Enjoyment_V1', 'QOL:Meaningful_Life_V1', 'QOL:Concentration_V1', 'QOL:Accept_Bodily_Appearance_V1',
         'QOL:Negative_Feelings_Frequency_V1', 'QOL:Ability_Satisfaction_V1']].apply(pd.to_numeric,
                                                                                     errors='coerce').replace(888,
                                                                                                              np.nan).sum(
        axis=1, skipna=False)
    d3 = nsi[['QOL:Relationship_Satisfaction_V1', 'QOL:Sex_Life_Satisfaction_V1', 'QOL:Friend_Satisfaction_V1']].apply(
        pd.to_numeric, errors='coerce').replace(888, np.nan).sum(axis=1, skipna=False)
    d4 = nsi[['QOL:Feel_Safe_V1', 'QOL:Healthy_Physcial_Environmnet_V1', 'QOL:Money_to_Meet_Needs_V1',
              'QOL:Information_to_Meet_Needs_V1', 'QOL:Leisure_Activities_Opportunity_V1',
              'QOL:Living_Condition_Satisfaction_V1',
              'QOL:Health_Service_Access_V1', 'QOL:Transporation_Satisfaction_V1']].apply(pd.to_numeric,
                                                                                          errors='coerce').replace(888,
                                                                                                                   np.nan).sum(
        axis=1, skipna=False)

    nsi["WHOQOL_Physical_Score"] = ((d1 - 7) / 28) * 100
    nsi["WHOQOL_Psychological_Score"] = ((d2 - 6) / 24) * 100
    nsi["WHOQOL_Social_Relationships_Score"] = ((d3 - 3) / 12) * 100
    nsi["WHOQOL_Enviornment_Score"] = ((d4 - 8) / 32) * 100
    return nsi


### Thresholds for PSQI Component 2
def f(x):
    if x <= 15:
        return 0
    elif x > 15 and x <= 30:
        return 1
    elif x > 30 and x <= 60:
        return 2
    elif x > 60:
        return 3
    else:
        return np.nan


### Thresholds for PSQI Component 2, and Component 7
def s(x):
    if x == 0:
        return 0
    elif x > 0 and x <= 2:
        return 1
    elif x > 2 and x <= 4:
        return 2
    elif x > 4:
        return 3
    else:
        return np.nan


### Thresholds for PSQI Component 3
def p(x):
    if x > 7:
        return 0
    elif x <= 7 and x >= 6:
        return 1
    elif x < 6 and x >= 5:
        return 2
    elif x < 5:
        return 3
    else:
        return np.nan


### Thresholds for PSQI Component 4
def e(x):
    if x >= 0.85:
        return 0
    elif x < 0.85 and x >= 0.75:
        return 1
    elif x < 0.75 and x >= 0.65:
        return 2
    elif x < 0.65:
        return 3
    else:
        return np.nan


### Thresholds for PSQI Component 5
def q(x):
    if x == 0:
        return 0
    elif x > 0 and x <= 9:
        return 1
    elif x > 9 and x <= 18:
        return 2
    elif x > 18:
        return 3
    else:
        return np.nan


### scoring for the PSQI
### output: a data frame that includes total score column for the PSQI
###PSQI Scoring
def psqi_scoring(nsi):
    ## Component 1
    C1 = pd.to_numeric(nsi['PSQI:Overall_Sleep_Quality_V1'], errors="coerce").replace(888, np.nan)
    ## Component 2
    q2 = nsi['PSQI:Time_to_Fall_Sleep_V1']
    q2 = pd.to_numeric(q2, errors="coerce").replace(888, np.nan).apply(f)
    C2 = q2.add(nsi['PSQI:Sleep_Within_30Minutes_V1'], fill_value=0).apply(s)
    ## Component 3
    C3 = pd.to_numeric(nsi['PSQI:Hours_of_Sleep_V1'], errors="coerce").replace(888, np.nan).apply(p)
    ## Component 4
    C4 = pd.to_numeric(nsi['PSQI:Hours_of_Sleep_V1'], errors="coerce").replace(888, np.nan) * 100 / (2400 - (
                pd.to_numeric(nsi['PSQI:Bed_Time_V1'], errors='coerce').replace(888, np.nan) - pd.to_numeric(
            nsi['PSQI:WakeUp_Time_V1'], errors='coerce').replace(888, np.nan)))
    C4 = C4.apply(e)
    ## Component 5
    C5cols = ['PSQI:WakeUp_at_Night_V1', 'PSQI:Bathroom_at_Night_V1', 'PSQI:Cannot_Breathe_Comfortably_V1',
              'PSQI:Cough_or_Snore_V1', 'PSQI:Feel_Too_Cold_V1', 'PSQI:Feel_Too_Hot_V1', 'PSQI:Bad_Dreams_V1',
              'PSQI:Pain_V1', 'PSQI:Other_Reasons_V1']
    C5 = nsi[C5cols].apply(pd.to_numeric, errors="coerce").replace(888, np.nan).sum(axis=1, min_count=1).apply(q)
    ## Component 6
    C6 = pd.to_numeric(nsi['PSQI:Medicine_Aided_Sleep_V1'], errors="coerce").replace(888, np.nan)
    ## Component 7
    C7cols = ['PSQI:Trouble_Staying_Awake_V1', 'PSQI:Enthusiasm_V1']
    C7 = nsi[C7cols].apply(pd.to_numeric, errors="coerce").replace(888, np.nan).sum(axis=1, min_count=1).apply(s)

    nsi['PSQI_Score'] = C1 + C2 + C3 + C4 + C5 + C6 + C7
    return nsi


### read in NICoE data
### output: returns a data frame that contains additional rows of NICoE surveys
def add_nicoe(nsi, nicoe):
    nicoe['NSI_TotalScore'] = nicoe[
        ['NSI:Anxious_V1', 'NSI:Appetite_Change_V1', 'NSI:Balance_V1', 'NSI:Clumsy_V1', 'NSI:Decision_Difficulty_V1',
         'NSI:Depressed_V1', 'NSI:Distracted_V1', 'NSI:Dizzy_V1', 'NSI:Fatigue_V1', 'NSI:Forgetfulness_V1',
         'NSI:Headaches_V1', 'NSI:Hearing_Difficulty', 'NSI:Irritability_V1', 'NSI:Light_Sensitivity_V1',
         'NSI:Nausea_V1', 'NSI:Noise_Sensitivity_V1', 'NSI:Numbness_V1', 'NSI:Overwhelmed_V1',
         'NSI:Sleep_Difficulty_V1', 'NSI:Slow_Thinking_V1', 'NSI:Taste_Smell_Change_V1',
         'NSI:Vision_Problems_V1']].apply(pd.to_numeric, errors='coerce').replace(888, np.nan).sum(axis=1, skipna=False)
    nicoe = nicoe.dropna(subset=['NSI_TotalScore'])
    nicoe = nicoe.loc[:, ~nicoe.columns.duplicated()]
    nsi = nsi.loc[:, ~nsi.columns.duplicated()]
    nsi = nsi.append(nicoe, ignore_index=True)
    nsi['parent_dmis_key_name'] = np.where(pd.isnull(nsi['parent_dmis_key_name']), nsi['clinic'],
                                           nsi['parent_dmis_key_name'])
    nsi['parent_dmis_key_name'] = nsi['parent_dmis_key_name'].replace('Fort Belvior', 'FT. BELVOIR-PDMIS')
    return nsi


### scoring for the NSI Factor Scores
### output: a data frame that includes total score columns for the NSI Factor Scores
### Factor Scores
def add_factor_score(nsi):
    nsi['NSI_F1_Vestibular'] = nsi[['NSI:Clumsy_V1', 'NSI:Dizzy_V1', 'NSI:Balance_V1']].mean(axis=1)
    nsi['NSI_F2_Cognative'] = nsi[
        ['NSI:Distracted_V1', 'NSI:Forgetfulness_V1', 'NSI:Decision_Difficulty_V1', 'NSI:Slow_Thinking_V1']].mean(
        axis=1)
    nsi['NSI_F3_Affective'] = nsi[
        ['NSI:Fatigue_V1', 'NSI:Sleep_Difficulty_V1', 'NSI:Anxious_V1', 'NSI:Depressed_V1', 'NSI:Irritability_V1',
         'NSI:Overwhelmed_V1']].mean(axis=1)
    nsi['NSI_F4_Sensory'] = nsi[
        ['NSI:Headaches_V1', 'NSI:Nausea_V1', 'NSI:Vision_Problems_V1', 'NSI:Light_Sensitivity_V1',
         'NSI:Noise_Sensitivity_V1', 'NSI:Numbness_V1', 'NSI:Taste_Smell_Change_V1']].mean(axis=1)
    nsi['NSI_Validity_10'] = nsi[
        ['NSI:Dizzy_V1', 'NSI:Balance_V1', 'NSI:Clumsy_V1', 'NSI:Nausea_V1', 'NSI:Vision_Problems_V1',
         'NSI:Noise_Sensitivity_V1', 'NSI:Hearing_Difficulty', 'NSI:Taste_Smell_Change_V1', 'NSI:Slow_Thinking_V1',
         'NSI:Decision_Difficulty_V1']].sum(axis=1)
    return nsi


### read in BAMC data
### output: returns a data frame that contains additional rows of BAMC surveys
def add_bamc(nsi, bamc):
    bamc = bamc.rename(columns={"Patient_ID": "edi"})
    nsi = nsi.append(bamc, ignore_index=True)
    nsi = nsi.dropna(subset=['NSI_TotalScore'])
    return nsi


### get validity-10 per dmis, and remove NSIs with a Validity 10 of 22 or more
### output: a data frame that has validity 10s of 22 or more removed. As well, it prints a table of DMISs that includes
### dmis name, count of validity 10, precentage validity 10, count of all NSIs
def validity_10(nsi):
    for dmis in list(nsi['parent_dmis_key_name'].unique()):
        subdf = nsi[nsi['parent_dmis_key_name'] == dmis]
        subdf.replace(888, np.nan, inplace=True)
        if len(subdf) > 10:
            print(dmis.replace(" ", "_"), str(
                (subdf["NSI_Validity_10"][subdf["NSI_Validity_10"] >= 22].count() / len(subdf) * 100).round(2)) + "%",
                  subdf["NSI_Validity_10"][subdf["NSI_Validity_10"] >= 22].count(), len(subdf))
    nsi = nsi[nsi["NSI_Validity_10"] < 22]
    return nsi


### Creates a CSV that show the average score of each survey by dmis. Requires an input of desired columns to be averaged
### output: a CSV that shows the average score of each survey by dmis
def Avg_score_by_dmis(avgscores, nsi):
    df = []
    for test in avgscores:
        df.append([test.replace("_", " "), nsi[test].mean()])
    outdf = pd.DataFrame(df, columns=['index', 'All']).set_index('index').round(2)
    for dmis in list(nsi['parent_dmis_key_name'].unique()):
        subdf = nsi[nsi['parent_dmis_key_name'] == dmis]
        subdf.replace(888, np.nan, inplace=True)
        subdf["start_date"] = pd.to_datetime(subdf["start_date"])
        subdf.sort_values(by=["edi", "start_date"], inplace=True)
        if len(subdf) > 10:
            df = []
            subdf.drop_duplicates(subset=["edi"], keep="first", inplace=True)
            for test in avgscores:
                df.append([test.replace("_", " "), subdf[test].mean()])
            df = pd.DataFrame(df, columns=['index', dmis]).set_index('index')
            outdf = outdf.join(df).round(2)
    outdf.to_csv("AverageScorebyDMIS.csv")


### Takes the NSI data frame and the manually created total scores desired, and compares them. It gets the pearson R values, and the Pvalue for all and
### for each dmis. It checks to make sure there were at least 10 NSI surveys conducted at the DMIS before it returns a value.
### Output: A TXT file of tables of correlations and significances based on DMIS. It returns both a table with astics and a table with clean values
### porting to excel
def get_dmis_tables(totscores, nsi):
    ppt = open("NSI_byDMIS_Tables.txt", "a")
    for name, comps in totscores.items():
        print(name)
        print("****************************", file=ppt)
        print(name, file=ppt)
        df = []
        dfheat = []
        for combo in comps:
            mask = ~np.isnan(pd.to_numeric(nsi[combo[0]], errors='coerce')) & ~np.isnan(
                pd.to_numeric(nsi[combo[1]], errors='coerce'))
            try:
                slope, intercept, rvalue, pvalue, stderr = linregress(
                    pd.to_numeric(nsi[combo[0]], errors='coerce')[mask],
                    pd.to_numeric(nsi[combo[1]], errors='coerce')[mask])
            except:
                continue
            if pvalue < 0.05:
                df.append([combo[0], str('{:5f}'.format(rvalue)) + "*"])
            else:
                df.append([combo[0], str('{:5f}'.format(rvalue))])
            dfheat.append([combo[0], str('{:5f}'.format(rvalue))])
        df.append([name + "_Total_N", len(mask[mask])])
        df = pd.DataFrame(df, columns=['index', 'All']).set_index('index')
        print(df)
        dfheat = pd.DataFrame(dfheat, columns=['index', 'All']).set_index('index')
        outdf = df
        outheat = dfheat
        for dmis in list(nsi['parent_dmis_key_name'].unique()):
            subdf = nsi[nsi['parent_dmis_key_name'] == dmis]
            subdf.replace(888, np.nan, inplace=True)
            if len(subdf) > 10:
                df = []
                dfheat = []
                for combo in comps:
                    mask = ~np.isnan(pd.to_numeric(nsi[combo[0]], errors='coerce')) & ~np.isnan(
                        pd.to_numeric(nsi[combo[1]], errors='coerce'))
                    try:
                        slope, intercept, rvalue, pvalue, stderr = linregress(
                            pd.to_numeric(subdf[combo[0]], errors='coerce')[mask],
                            pd.to_numeric(subdf[combo[1]], errors='coerce')[mask])
                    except:
                        continue
                    if pvalue < 0.05:
                        df.append([combo[0], str('{:5f}'.format(rvalue)) + "*"])
                    else:
                        df.append([combo[0], str('{:5f}'.format(rvalue))])
                    dfheat.append([combo[0], str('{:5f}'.format(rvalue))])
                df.append([name + "_Total_N", len(subdf[combo[1]][mask])])
                df = pd.DataFrame(df, columns=['index', dmis.replace(" ", "_")]).set_index('index')
                dfheat = pd.DataFrame(dfheat, columns=['index', dmis.replace(" ", "_")]).set_index('index')
                outdf = outdf.join(df)
                outheat = outheat.join(dfheat)
        print(outdf, file=ppt)
        print('*****', file=ppt)
        print(outheat, file=ppt)
    ppt.close()


if __name__ == "__main__":
    ### Comparisons for the DMIS Tables
    totscores = {
        "HIT-6": [("NSI_TotalScore", "HIT6_Score"), ('NSI_F1_Vestibular', "HIT6_Score"),
                  ('NSI_F2_Cognative', "HIT6_Score"), ('NSI_F3_Affective', "HIT6_Score"),
                  ('NSI_F4_Sensory', "HIT6_Score"), ('NSI:Headaches_V1', "HIT6_Score")],
        "PHQ9": [("NSI_TotalScore", "PHQ9_Score"), ('NSI_F1_Vestibular', "PHQ9_Score"),
                 ('NSI_F2_Cognative', "PHQ9_Score"), ('NSI_F3_Affective', "PHQ9_Score"),
                 ('NSI_F4_Sensory', "PHQ9_Score"), ("NSI:Depressed_V1", "PHQ9_Score")],
        "AUDIT": [("NSI_TotalScore", 'AUDIT_Score'), ('NSI_F1_Vestibular', 'AUDIT_Score'),
                  ('NSI_F2_Cognative', 'AUDIT_Score'), ('NSI_F3_Affective', 'AUDIT_Score'),
                  ('NSI_F4_Sensory', 'AUDIT_Score'), ("NSI:Depressed_V1", "AUDIT_Score")],
        "PSQI": [("NSI_TotalScore", "PSQI_Score"), ('NSI_F1_Vestibular', "PSQI_Score"),
                 ('NSI_F2_Cognative', "PSQI_Score"), ('NSI_F3_Affective', "PSQI_Score"),
                 ('NSI_F4_Sensory', "PSQI_Score"), ('NSI:Sleep_Difficulty_V1', "PSQI_Score")],
        "ISI": [("NSI_TotalScore", 'ISI_Score'), ('NSI_F1_Vestibular', 'ISI_Score'), ('NSI_F2_Cognative', 'ISI_Score'),
                ('NSI_F3_Affective', 'ISI_Score'), ('NSI_F4_Sensory', 'ISI_Score'),
                ('NSI:Sleep_Difficulty_V1', "ISI_Score")],
        "GAD-7": [("NSI_TotalScore", 'GAD_Score'), ('NSI_F1_Vestibular', 'GAD_Score'),
                  ('NSI_F2_Cognative', 'GAD_Score'), ('NSI_F3_Affective', 'GAD_Score'), ('NSI_F4_Sensory', 'GAD_Score'),
                  ('NSI:Anxious_V1', "GAD_Score")],
        "ABC": [("NSI_TotalScore", 'abc_tot_score'), ('NSI_F1_Vestibular', 'abc_tot_score'),
                ('NSI_F2_Cognative', 'abc_tot_score'), ('NSI_F3_Affective', 'abc_tot_score'),
                ('NSI_F4_Sensory', 'abc_tot_score'), ('NSI:Balance_V1', 'abc_tot_score')],
        "DHI": [("NSI_TotalScore", 'dhi_tot_score'), ('NSI_F1_Vestibular', 'dhi_tot_score'),
                ('NSI_F2_Cognative', 'dhi_tot_score'), ('NSI_F3_Affective', 'dhi_tot_score'),
                ('NSI_F4_Sensory', 'dhi_tot_score'), ('NSI:Dizzy_V1', 'dhi_tot_score')],
        "Epworth": [("NSI_TotalScore", 'epworth_tot_score'), ('NSI_F1_Vestibular', 'epworth_tot_score'),
                    ('NSI_F2_Cognative', 'epworth_tot_score'), ('NSI_F3_Affective', 'epworth_tot_score'),
                    ('NSI_F4_Sensory', 'epworth_tot_score'), ('NSI:Fatigue_V1', "epworth_tot_score")],
        "PCL520": [("NSI_TotalScore", 'pcl520_tot_score'), ('NSI_F1_Vestibular', 'pcl520_tot_score'),
                   ('NSI_F2_Cognative', 'pcl520_tot_score'), ('NSI_F3_Affective', 'pcl520_tot_score'),
                   ('NSI_F4_Sensory', 'pcl520_tot_score')],
        "PCL-M": [("NSI_TotalScore", 'pclm_tot_score'), ('NSI_F1_Vestibular', 'pclm_tot_score'),
                  ('NSI_F2_Cognative', 'pclm_tot_score'), ('NSI_F3_Affective', 'pclm_tot_score'),
                  ('NSI_F4_Sensory', 'pclm_tot_score')],
        "WHOQOL": [("WHOQOL_Physical_Score", "NSI_TotalScore"), ("WHOQOL_Psychological_Score", "NSI_TotalScore"),
                   ("WHOQOL_Social_Relationships_Score", "NSI_TotalScore"),
                   ("WHOQOL_Enviornment_Score", "NSI_TotalScore")]
    }

    ### Columns to be compared in Average scoring
    avgscores = ["NSI_TotalScore", 'NSI_F1_Vestibular', 'NSI_F2_Cognative', 'NSI_F3_Affective', 'NSI_F4_Sensory',
                 "HIT6_Score", "PHQ9_Score", 'AUDIT_Score', "PSQI_Score", 'ISI_Score', 'GAD_Score', 'abc_tot_score',
                 "dhi_tot_score", 'epworth_tot_score', 'pcl520_tot_score', 'pclm_tot_score', "WHOQOL_Physical_Score",
                 "WHOQOL_Psychological_Score", "WHOQOL_Social_Relationships_Score", "WHOQOL_Enviornment_Score"]

    ### location of BHDP data
    mergedir = r"\\WRNMDFPISISMBD1\DeptShares$\Dept6\NICoE_Informatics\NICoE_Informatics\BHDP\ALL_DATA\Mergeing"
    ### Read in and clean the BHDP data
    nsi = read_bhdp(mergedir)

    ### Simple Additive Scoring for numerous surveys
    cols = list(nsi.columns)
    nsi = scoring_1(nsi, cols)
    ### WHOQOL Scoring, required sectioning of questions
    nsi = whoqol_scoring(nsi)
    ### PSQI Scoring, extremely complex. Required multiple steps, and sections
    nsi = psqi_scoring(nsi)

    ### Location of NICoE Data
    nicoe = pd.read_csv(
        r"\\wrnmdfpisismbd1\DeptShares$\Dept6\NICoE_Informatics\NICoE_Informatics\Ande\Analysis\NSI_Comp\NICOE_data_limitedCols.csv")
    ### Add in the NICoE Data
    nsi = add_nicoe(nsi, nicoe)

    ### Add in the NSI Factor Score
    nsi = add_factor_score(nsi)

    ### Add in the BAMC data
    bamc = pd.read_csv("BAMC_NSIComp_Manip.csv")
    nsi = add_bamc(nsi, bamc)

    ### validity 10 scoring per dmis and removal, some versions of the ppt included or removed Validity-10
    nsi = validity_10(nsi)

    ### print the average score by DMIS to csv
    Avg_score_by_dmis(avgscores, nsi)

    ### print the correlation tables to TXT
    get_dmis_tables(totscores, nsi)

