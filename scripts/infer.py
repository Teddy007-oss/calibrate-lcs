import joblib
import pandas as pd

#loading the model for dry period
model_dry = joblib.load(r"C:\Users\USER\Documents\Afri-SET\Calibrations\models\Clarity_Dry_mlr_model.joblib")
model_wet = joblib.load(r"C:\Users\USER\Documents\Afri-SET\Calibrations\models\Clarity_Wet_mlr_model.joblib")

df_dry = pd.read_csv(r"C:\Users\USER\Documents\Afri-SET\Calibrations\TBC_dry.csv",
                 parse_dates=['date'],
                 index_col = "date")

df_wet = pd.read_csv(r"C:\Users\USER\Documents\Afri-SET\Calibrations\TBC_wet.csv",
                 parse_dates=['date'],
                 index_col = "date")

sites = ["CPC", "St", "Bet", "Com", "TMA", "Fre", "Val",
         "LEK", "Chal", "Ade", "San", "Adu", "Sua", "Sep",
         "Aso"]

for site in sites:
    pm25 = f"{site}_pm25"
    temp = f"{site}_temp"
    rh = f"{site}_rh"
    
    if all(col in df_dry.columns for col in [pm25, temp, rh]):
        
        X_dry = df_dry[[pm25, temp, rh]].dropna()
        X_dry = X_dry.rename(columns={pm25: "pm25", temp: "temp", rh: "rh"})
        corrected_dry = model_dry.predict(X_dry)
        df_dry.loc[X_dry.index, f"{site}_corrected"] = corrected_dry
        
    if all (col in df_wet.columns for col in [pm25, temp, rh]):   
        
        X_wet = df_wet[[pm25, temp, rh]].dropna()
        X_wet = X_wet.rename(columns={pm25: "pm25", temp: "temp", rh: "rh"})
        corrected_wet = model_wet.predict(X_wet)
        df_wet.loc[X_wet.index, f"{site}_corrected"] = corrected_wet

    else:
        print(f"Missing columns for site {site}. Skipping...")

# Convert index to datetime safely
df_dry.index = pd.to_datetime(df_dry.index, errors="coerce", format="mixed")
df_wet.index = pd.to_datetime(df_wet.index, errors="coerce", format="mixed")

# Then force a consistent string format
df_dry.index = df_dry.index.strftime("%Y-%m-%d %H:%M:%S")
df_wet.index = df_wet.index.strftime("%Y-%m-%d %H:%M:%S")
        
df_dry.to_csv(r"C:\Users\USER\Documents\Afri-SET\Calibrations\clarity_dry_corrected.csv", index = True)
df_wet.to_csv(r"C:\Users\USER\Documents\Afri-SET\Calibrations\clarity_wet_corrected.csv", index = True)

print("Inference completed and results saved.")