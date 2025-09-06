import pandas as pd
from sklearn.linear_model import LinearRegression
from sklearn.model_selection import cross_val_score, KFold 
from joblib import dump

def mlr_calibration(df, features, target, sensor_name):
    """
    Perform Multiple Linear Regression (MLR) calibration.

    Parameters:
    df (pd.DataFrame): DataFrame containing the features and target variable.
    features (list): List of feature column names.
    target (str): Target variable column name.
    sensor_name (str): Name of the sensor for saving the model.

    Returns:
    Coeff_model (LinearRegression): Trained Linear Regression model.
    """
    #prepare the data
    X = features
    y = target
    
    model = LinearRegression()
    kf = KFold(n_splits = 5, shuffle = True, random_state = 42)
    cv_scores = cross_val_score(model, X, y, cv = kf, scoring = 'r2')
    
    model.fit(X, y)
    
    
    # Save the model
    dump(model, f'models/{sensor_name}_mlr_model.joblib')
    
    for feature, coef in zip(features.columns, model.coef_):
        print(f'Coefficient for {feature}: {coef:.4f}')
    print(f'Intercept: {model.intercept_:.4f}')
    
    #prediction
    predictions = model.predict(X)
    df[f'{sensor_name}_cal'] = predictions
    
    return model, cv_scores
    
    
if __name__ == "__main__":
    
    # file path must be changed to the location of the data
    df = pd.read_csv(
        r"C:\Users\USER\Documents\Afri-SET\Calibrations\clarity_dry.csv",
        parse_dates=["date"],
        index_col="date"
    )
     
    columns_mapping = {
        "pm25": ["pm25a", "pm25b", "pm25c"],
        "rh": ["rha", "rhb", "rhc"],    
        "temp": ["tempa", "tempb", "tempc"]
    }
    
    for avg_col, source_cols in columns_mapping.items():
        if avg_col not in df.columns:
            df[avg_col] = df[source_cols].mean(axis=1)
            
    df = df[["PM2.5", "pm25", "temp", "rh"]].dropna()
    
    
    """if df["pm25", "rh", "temp"] not in df.columns:
        
        #Averaging the LCS PM data
        df["pm25"] = df[["pm25a", "pm25b", "pm25c"]].mean(axis=1)
        df["rh"] = df[["rha", "rhb", "rhc"]].mean(axis=1)
        df["temp"] = df[["tempa", "tempb", "tempc"]].mean(axis=1)
        
        #keeping only the relevant columns
        df = df[["PM2.5", "pm25", "temp", "rh"]].dropna()
        
    else:
        #keeping only the relevant columns when the averaging is already done
        df = df[["PM2.5", "pm25", "temp", "rh"]].dropna()"""
    
    
    #defining the reference and co
    target = df["PM2.5"]
    features = df[["pm25","temp", "rh"]]
    
    #the period for the calibration must be added to the sensor name, say sensit_ramp_wet
    mlr_calibration(df, features, target, sensor_name="Clarity_Dry")
    
    #Be sure the state the sensor name
    sensor_name = "Clarity_Dry" 
    outdir = r"C:\Users\USER\Documents\Afri-SET\Calibrations\data"
    df.to_csv(f"{outdir}/{sensor_name}_calibrated_data.csv")
    
    
    
