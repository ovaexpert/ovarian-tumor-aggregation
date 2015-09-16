computeWeightedSum = function(patient_row, cols, coeffs, value.min, value.max){
    result_min = 0;
    result_max = 0;
    for (i in 1:length(cols)) {
        coeff = coeffs[[i]]
        attr = cols[[i]]
        if (is.na(patient_row[[attr]])) {
            if(coeff>0) {
                if(!is.null(value.min[[attr]])){
                    result_min = result_min + coeff * value.min[[attr]]
                }
                if(!is.null(value.max[[attr]])){
                    result_max = result_max + coeff * value.max[[attr]]
                }else{
                    result_max = result_max + coeff
                }
            } else {
                if(!is.null(value.min[[attr]])){
                    result_max = result_max + coeff * value.min[[attr]]
                }
                if(!is.null(value.max[[attr]])){
                    result_min = result_min + coeff * value.max[[attr]]
                }else{
                    result_min = result_min + coeff
                }
            }
        } else {
            value = patient_row[[attr]]
            if(!is.null(value.min[[attr]])){
                value = max(value,value.min[[attr]])
            }
            if(!is.null(value.max[[attr]])){
                value = min(value,value.max[[attr]])
            }
            result_min = result_min + coeff*value
            result_max = result_max + coeff*value
        }
    }
    return(c(result_min,result_max))
}

normalizeValue = function(x, val.min, val.mid, val.max) {
    if(x>val.max || x<val.min) {
        warning("Value ",x," out of bounds (",val.min,", ",val.max,").")
    }
    if(x< val.mid){
        return(0.5*(x-val.min)/(val.mid-val.min))
    }else{
        return(0.5+0.5*((x-val.mid)/(val.max-val.mid)))
    }
}


MNP.COLS = c('AgeAfterMenopause')
MNP = function(patient_row) {
    if(patient_row[['AgeAfterMenopause']]>0){
        return(c(1,1))
    }else{
        return(c(0,0))
    }
}

SM.COLS = c("Septum", "SmEchogenicity", "Location", "Ascites", "SmInnerWallThickness",
            "TumorVolume", "Solid", "Pap", "APapDimension", "InnerWall", "SeptumThickness",
            "AgeAfterMenopause")
SM = function(patient_row) {
    result_min = 0
    result_max = 0

    if(is.na(patient_row[['Ascites']])){
        result_max = result_max + 2
    }else{
        if(patient_row[['Ascites']]==1){
            result_min = result_min + 2
            result_max = result_max + 2
        }
    }

    if(is.na(patient_row[['Solid']])){
        result_max = result_max + 3
    } else {
        if( patient_row[["Solid"]]==1){
            result_min = result_min + 3
            result_max = result_max + 3
        }else{
            if(is.na(patient_row[['APapDimension']])){
                result_max = result_max + 2
            }else{
                if(patient_row[['APapDimension']]>=3){
                    result_min = result_min + 2
                    result_max = result_max + 2
                }else{
                    if(is.na(patient_row[['InnerWall']])){
                        result_max = result_max + 1
                    }else{
                        if(patient_row[['InnerWall']]==1){
                            result_min = result_min + 1
                            result_max = result_max + 1
                        }
                    }
                }
            }
        }
    }

    if(is.na(patient_row[['Septum']])){
        result_max = result_max + 2
    } else{
        if(patient_row[['Septum']]>0){
            if(is.na(patient_row[['SeptumThickness']])){
                result_min = result_min + 1
                result_max = result_max + 2
            }else{
                points = 0
                if(patient_row[['SeptumThickness']]<=3){
                    points = 1
                }else{
                    points = 2
                }
                result_min = result_min + points
                result_max = result_max + points
            }
        }
    }

    if(is.na(patient_row[["SmEchogenicity"]])){
        result_max = result_max + 4
    }else{
        result_min = result_min + patient_row[["SmEchogenicity"]]
        result_max = result_max + patient_row[["SmEchogenicity"]]
    }

    if(is.na(patient_row[["Location"]])){
        result_max = result_max + 1
    }else{
        points = 0
        if(patient_row[["Location"]]>0){
            points = 1
        }
        result_min = result_min + points
        result_max = result_max + points
    }

    if(is.na(patient_row[["SmInnerWallThickness"]])){
        result_max = result_max + 2
    }else{
        result_min = result_min + patient_row[["SmInnerWallThickness"]]
        result_max = result_max + patient_row[["SmInnerWallThickness"]]
    }

    if (is.na(patient_row[['TumorVolume']]) || is.na(patient_row[['AgeAfterMenopause']])) {
        result_max = result_max + 4
    } else {
        points = NA
        if ( (patient_row[['AgeAfterMenopause']] == 0 &&  patient_row[['TumorVolume']] < 20 ) ||
                 (patient_row[['AgeAfterMenopause']] > 0 &&  patient_row[['TumorVolume']] < 10 ) )
            points = 0
        else if ( (patient_row[['AgeAfterMenopause']] == 0 &&  patient_row[['TumorVolume']] >= 20  &&  patient_row[['TumorVolume']] <= 50 ) ||
                      (patient_row[['AgeAfterMenopause']] >0 &&  patient_row[['TumorVolume']] >= 10  &&  patient_row[['TumorVolume']] <= 50) )
            points = 1
        else if ( patient_row[['TumorVolume']] > 50  &&  patient_row[['TumorVolume']] <= 200 )
            points = 2
        else if ( patient_row[['TumorVolume']] > 200  &&  patient_row[['TumorVolume']] <= 500 )
            points = 3
        else # if (patient_row[['TumorVolume']] > 500 )
            points = 4

        result_min = result_min + points
        result_max = result_max + points
    }
    result_min = normalizeValue(result_min, 0, 8, 18)
    result_max = normalizeValue(result_max, 0, 8, 18)
    return(c("SM.min"=result_min, "SM.max"=result_max))
}


SD.COLS = c('SdVascularNumber','SdVascularNumber','SdVascularLocation','SdVasculature','SdAmplitude')
SD = function(patient_row) {
    result_min = 0
    result_max = 0
    for(attr in SD.COLS){
        if(is.na(patient_row[[attr]])) {
            result_max = result_max + 1
        }else{
            result_min = result_min+patient_row[[attr]]
            result_max = result_max+patient_row[[attr]]
        }
    }
    return(c("SD.min"=result_min, "SD.max"=result_max))
}

LR1.COLS = c("OvarianCancerInFamily", "HormoneReplacementTherapy", "Age",
             "ADimension", "PainAtExamination", "Ascites", "PapBloodFlow",
             "Solid", "ASolidDimension", "InnerWall", "Shadow", "Color")
LR1 = function(patient_row){
    coeffs = c(1.5985, -0.9983, 0.0326, 0.00841, -0.8577, 1.5513, 1.1737, 0.9281,
              0.0496, 1.1421, -2.355, 0.4916)
    value.min = list("Age"=10, "ADimension"=20, "Color"=1)
    value.max = list("Age"=100, "ADimension"=200, "ASolidDimension"=50, "Color"=4)

    val.min = -9.9720
    val.max = 9.0353
    val.mid = -2.1972

    tmp = computeWeightedSum(patient_row, LR1.COLS, coeffs, value.min, value.max)

    result_min = normalizeValue(tmp[[1]] - 6.7468, val.min, val.mid, val.max)
    result_max = normalizeValue(tmp[[2]] - 6.7468, val.min, val.mid, val.max)

    return(c("LR1.min"=result_min, "LR1.max"=result_max))
}

LR2.COLS = c("Age", "Ascites", "PapBloodFlow", "ASolidDimension", "InnerWall", "Shadow")
LR2 = function(patient_row){
    coeffs = c(0.0354, 1.6159, 1.1768, 0.0697, 0.9586, -2.9486)
    value.min = list("Age"=10)
    value.max = list("Age"=100, "ASolidDimension"=50)

    val.min = -7.9664
    val.max = 5.4045
    val.mid = -2.1972

    tmp = computeWeightedSum(patient_row, LR2.COLS, coeffs, value.min, value.max)

    result_min = normalizeValue(tmp[[1]] - 5.3718, val.min, val.mid, val.max)
    result_max = normalizeValue(tmp[[2]] - 5.3718, val.min, val.mid, val.max)

    return(c("LR2.min"=result_min, "LR2.max"=result_max))
}

# Timmermann scale uses APapDimension>3 instead of Pap
TIM.COLS = c("Color", "Ca125", "APapDimension", "AgeAfterMenopause")
TIM = function(patient_row) {
    val.min = -11.0427
    val.max = 17.8665
    val.mid = 0

    result_min=0
    result_max=0

    if(is.na(patient_row[['Color']])){
        result_min = result_min + 1*2.6369
        result_max = result_max + 4*2.6369
    }else{
        result_min=result_min + patient_row[['Color']] * 2.6369
        result_max=result_max + patient_row[['Color']] * 2.6369
    }
    if(is.na(patient_row[['Ca125']])){
        result_max = result_max + 500 * 0.0225
    }else{
        result_min=result_min + min(500,patient_row[['Ca125']]) * 0.0225
        result_max=result_max + min(500,patient_row[['Ca125']]) * 0.0225
    }

    if(is.na(patient_row[['AgeAfterMenopause']])){
        result_max = result_max + 2.6423
    }else{
        if(patient_row[['AgeAfterMenopause']]>0){
            result_min=result_min + 2.6423
            result_max=result_max + 2.6423
        }
    }
    if(is.na(patient_row[['APapDimension']])){
        result_max = result_max + 7.1062
    }else{
        if(patient_row[['APapDimension']]>3){
            result_min=result_min + 7.1062
            result_max=result_max + 7.1062
        }
    }

    result_min = normalizeValue(result_min - 13.6796, val.min, val.mid, val.max)
    result_max = normalizeValue(result_max - 13.6796, val.min, val.mid, val.max)

    return(c("Tim.min"=result_min, "Tim.max"=result_max))
}

ALC.COLS = c("APapDimension", "PapBloodFlow","Ri","ASolidDimension","Solid")
ALC = function(patient_row) {
    result_min = 0
    result_max = 0

    if (is.na(patient_row[['APapDimension']])) {
        result_max = result_max + 2
    } else {
        if (patient_row[['APapDimension']] >= 3){
            result_min = result_min + 2
            result_max = result_max + 2
        }
    }

    if (is.na(patient_row[['PapBloodFlow']])) {
        result_max = result_max + 4
    } else {
        if (patient_row[['PapBloodFlow']] == 1) {
            result_min = result_min + 4
            result_max = result_max + 4
        }
    }

    if (is.na(patient_row[['Ri']])) {
        result_max = result_max + 2
    } else {
        if (patient_row[['Ri']] <= 0.45) {
            result_min = result_min + 2
            result_max = result_max + 2
        }
    }

    if ( is.na(patient_row[['ASolidDimension']]) && is.na(patient_row[['Solid']])) {
        result_max = result_max + 4
    } else {
        points = 0
        if ( (!is.na(patient_row[['ASolidDimension']]) && patient_row[['ASolidDimension']] >= 10)
             || (!is.na(patient_row[['Solid']]) && patient_row[['Solid']] == 1 )) {
            points = 4
        }
        result_min = result_min + points
        result_max = result_max + points
    }
    return(c("Alc.min"=result_min/12, "Alc.max"=result_max/12))
}

RMI.COLS = c("AgeAfterMenopause","Age","UterusRemoved","IotaQuality",
             "ASolidDimension","Location","Ascites",'Ca125')
RMI = function(patient_row){
    if(!is.na(patient_row[['AgeAfterMenopause']]) && patient_row[['AgeAfterMenopause']]>0){ # 3 cases
        rmi_m_min = 1
        rmi_m_max = 1
    }else{
        if(!is.na(patient_row[['Age']]) && !is.na(patient_row[['UterusRemoved']])
           && patient_row[['Age']]>50 && patient_row[['UterusRemoved']]>=1){ #1 case
            rmi_m_min=1
            rmi_m_max=1
        }else{
            if(!is.na(patient_row[['AgeAfterMenopause']]) && !is.na(patient_row[['Age']])
               && patient_row[['AgeAfterMenopause']]==0 && patient_row[['Age']]<=50){
                rmi_m_min=0
                rmi_m_max=0
            }else{
                if(!is.na(patient_row[['AgeAfterMenopause']]) && !is.na(patient_row[['UterusRemoved']])
                   && patient_row[['AgeAfterMenopause']]==0 && patient_row[['UterusRemoved']]==0){
                    rmi_m_min=0
                    rmi_m_max=0
                }else{
                    rmi_m_min=0
                    rmi_m_max=1
                }
            }
        }
    }
    rmi_u_min = 0
    rmi_u_max = 0

    if(is.na(patient_row[['IotaQuality']])){
        rmi_u_max = rmi_u_max + 1
    }else{
        if(patient_row[['IotaQuality']]==3 || patient_row[['IotaQuality']]==4){
            rmi_u_max= rmi_u_max + 1
            rmi_u_min= rmi_u_min + 1
        }
    }
    if(is.na(patient_row[['ASolidDimension']])){
        rmi_u_max = rmi_u_max + 1
    }else{
        if(patient_row[['ASolidDimension']]>0){
            rmi_u_max= rmi_u_max + 1
            rmi_u_min= rmi_u_min + 1
        }
    }
    if(is.na(patient_row[['Location']])){
        rmi_u_max = rmi_u_max + 1
    }else{
        if(patient_row[['Location']]==1){
            rmi_u_max= rmi_u_max + 1
            rmi_u_min= rmi_u_min + 1
        }
    }
    if(is.na(patient_row[['Ascites']])){
        rmi_u_max = rmi_u_max + 1
    }else{
        if(patient_row[['Ascites']]==1){
            rmi_u_max= rmi_u_max + 1
            rmi_u_min= rmi_u_min + 1
        }
    }

    rmi_u_min=min(2,rmi_u_min)
    rmi_u_max=min(2,rmi_u_max)

    if(rmi_u_min>=2){
        rmi_u_min=3
    }
    if(rmi_u_max>=2){
        rmi_u_max=3
    }
    if(rmi_m_min>0){
        rmi_m_min=3
    }else{
        rmi_m_min=1
    }
    if(rmi_m_max>0){
        rmi_m_max=3
    }else{
        rmi_m_max=1
    }

    if(is.na(patient_row[['Ca125']])){
        result_max = 500*rmi_u_max*rmi_m_max
        result_min = 0
    }else{
        result_min = min(500,patient_row[['Ca125']])*rmi_u_min*rmi_m_min
        result_max = min(500,patient_row[['Ca125']])*rmi_u_max*rmi_m_max
    }

    val.min = 0
    val.max = 4500
    val.mid = 200

    result_min = normalizeValue(result_min, val.min, val.mid, val.max)
    result_max = normalizeValue(result_max, val.min, val.mid, val.max)

    return(c("RMI.min"=result_min, "RMI.max"=result_max))
}
# AVAILABLE DIAGNOSTIC METHODS
# at least 2 methods must be defined
METHODS.NAME = c("LR1","LR2", "SM", "Tim", "Alc","RMI")
METHODS.COL = list(LR1.COLS, LR2.COLS, SM.COLS, TIM.COLS, ALC.COLS, RMI.COLS)
METHODS = c(LR1, LR2, SM, TIM, ALC, RMI)

COLS.ALL  = unique(c(METHODS.COL, recursive = TRUE))
COLS.SURE = c("HormoneReplacementTherapy", "Age", "PainAtExamination", "AgeAfterMenopause",
              "UterusRemoved")
COLS.OBSC = COLS.ALL[!COLS.ALL %in% COLS.SURE]