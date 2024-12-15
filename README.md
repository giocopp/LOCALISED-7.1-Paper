
# **Manufacturing Decarbonization Risk Index: A Theoretical and Empirical Framework**

This project is derived from the Work Package 7: End-user Solutions for Regional Businesses and Investors of the [LOCALISED Project](https://www.localised-project.eu/). LOCALISED is an HORIZON 2020 European-funded research project aimed at providing downscaled decarbonisation trajectories, consistent with Europe’s net-zero target.

---

## **Project Structure**

This project includes a structured workflow divided into four pipelines for processing data related to exposure, vulnerability, response, and index calculation. Below is the detailed directory and file structure.

```
Project/
│
├── Pipeline 1: EXPOSURE
│   ├── run.R                      # R script for running the exposure pipeline
│   ├── targets.R                  # Script defining targets and steps
│   ├── targets/                   # Directory for target-related files
│   │   ├── ... (files)            
│   │
│   ├── Base_Data/                 
│   │   └── base_data.xlsx         # Input base data file
│   │
│   ├── Outputs/                   # Directory containing pipeline outputs
│   │   ├── Data/                  # Processed data outputs
│   │   │   └── ... (files)        
│   │   │
│   │   ├── Plots/                 # Plots and visualizations
│   │       └── ... (files)        
│   │
│   ├── R/                         # R-related files and scripts
│   │   ├── Excluded/              # Excluded or filtered-out data
│   │   │   └── ... (files)
│   │   │
│   │   └── ... (files)      
│   │
│
├── Pipeline 2: VULNERABILITY      # Next pipeline for assessing vulnerability
│   └── ... 
│
├── Pipeline 3: RESPONSE           # Pipeline for response assessment
│   └── ... 
│
└── Pipeline 4: INDEX              # Pipeline for index calculations
    └── ... 
```

---

## **Pipeline Descriptions**

### **Pipeline 1: Exposure**
This pipeline processes base data and produces cleaned data and visual outputs related to exposure metrics.

#### **Scripting Exposure Maps**

Below is the detailed workflow for scripting exposure maps using five key scripts:

1. **Eurostat_Empl_Pers**:  
   - Downloads and filters Eurostat employment data.

2. **Impute_Empl_Clean**:  
   - Imputes missing values using the **MICE** (Multiple Imputation by Chained Equations) method.

3. **Empl_Shares**:  
   - Calculates employment shares.  
   - Aggregates subsectors for analysis.

4. **Eurostat_Emissions**:  
   - Downloads and filters emissions data.  
   - Downscales emissions using employment shares calculated in the previous step.  
   - Outputs the final dataset.

5. **Exp_Map_RegEmis**:  
   - Generates exposure maps based on regional emissions data.

## **Workflow Diagram**
```
Eurostat_Empl_Pers
(Download & Filter Employment Data)
    │
    └── Impute_Empl_Clean
(Impute Missing Values with MICE)
                │
                └── Empl_Shares
(Calculate Employment Shares and Aggregate Sub-sectors)
                         │
                         └── Eurostat_Emissions
           (Download, Filter, and Downscale Emissions Data)
                                    │
                                    └── Uses Empl Shares
                                    (Outputs Final Dataset)
                                                │
                                                └── Exp_Map_RegEmis
                                     (Create Regional Emissions Exposure Maps)
```

## **Inputs**
- Employment data from Eurostat.
- Emissions data filtered and downscaled using employment shares.

## **Outputs**
- Final downscaled emissions dataset.
- Regional emissions exposure maps.


- **Inputs**:
  - `Emissions_Data_Raw`: Raw emissions data.
- **Outputs**:
  - Exposure raster maps at various stages.
  - Final downloadable layers for visualization.

---

### **Pipeline 2: Vulnerability**
This pipeline will handle the processing of data to assess vulnerabilities. (Details to be completed as the project progresses.)

---

### **Pipeline 3: Response**
This pipeline will analyze response-related metrics. (Details to be completed.)

---

### **Pipeline 4: Index**
This pipeline will calculate indexes based on prior outputs from exposure, vulnerability, and response pipelines. (Details to be completed.)

---

## **How to Run the Project**

1. **Set Up**: Ensure all required dependencies (e.g., R libraries) are installed.
2. **Execution**:
   - Navigate to `Pipeline 1` and run the `run.R` script to execute the exposure pipeline.
   - Outputs will be stored in the `Outputs/` directory.
3. **Next Steps**: Complete subsequent pipelines as needed.

---

This README now reflects the workflow for scripting exposure maps along with the overall project structure. Let me know if further adjustments are needed!
