
setwd("C:/sociometric-swapnil/mission antyodaya")

df2=read.csv("product_missionantyodaya2019_village_v0.csv",stringsAsFactors = F)

df3=read.csv("village_geom.csv",stringsAsFactors = F)

library(dplyr)
library(stringr)
library(tidyr)




df2$ma_state_code=as.numeric(gsub('.*\\(([0-9]+)\\).*?$', '\\1',df2$State_Name))
df2$ma_district_code=as.numeric(gsub('.*\\(([0-9]+)\\).*?$', '\\1', df2$District_Name))
df2$ma_block_code=as.numeric(gsub('.*\\(([0-9]+)\\).*?$', '\\1', df2$Development_Block_Name))  
df2$ma_gp_code=as.numeric(gsub('.*\\(([0-9]+)\\).*?$', '\\1', df2$Gram_Panchayat_Name))
df2$ma_village_code=as.numeric(gsub('.*\\(([0-9]+)\\).*?$', '\\1', df2$Village_Name))                     


df2$ma_state_name=trimws(gsub("\\s*\\([0-9]+\\)","",df2$State_Name))

df2$ma_district_name=trimws(gsub("\\s*\\([0-9]+\\)","",df2$District_Name))

df2$ma_block_name=trimws(gsub("\\s*\\([0-9]+\\)","",df2$Development_Block_Name))

df2$ma_gp_name=trimws(gsub("\\s*\\([0-9]+\\)","",df2$Gram_Panchayat_Name))

df2$ma_village_name=trimws(gsub("\\s*\\([0-9]+\\)","",df2$Village_Name))

df2=df2[,c(34,39,35,40,36,41,37,42,38,43,6:33)]

df4=left_join(df3,df2,by=c("village_code"="ma_village_code"))


df5=df4[!is.na(df4$ma_state_code),] #remove na values

df5=df5[,-c(12:21)]

df5[df5$village_code == '506545',]

df6=df5[!duplicated(df5),]#remove duplicated row from df

write.csv(df6,"mission_antyodaya_village.csv",row.names = F,na="")

#now filter those rows which didnt match
db= df3[!(df3$village_code %in% df6$village_code),]
write.csv(db,"villages_not_matched.csv",row.names=F,na="")

##########################################################################################

df6=read.csv("mission_antyodaya_village.csv",stringsAsFactors = F)

unique(df6$Availability_of_government_seed_centres)


df6$Availability_of_government_seed_centres_Yes= ifelse(df6$Availability_of_government_seed_centres %in% "Yes", 1, 0)
df6$`Availability_of_government_seed_centres_No_Nearest_facility_<1km`=ifelse(df6$Availability_of_government_seed_centres %in% "No ( Nearest facility< 1 km)", 1, 0)
df6$Availability_of_government_seed_centres_No_Nearest_facility_1_2kms=ifelse(df6$Availability_of_government_seed_centres %in% "No ( Nearest facility1-2 kms)", 1, 0)
df6$Availability_of_government_seed_centres_No_Nearest_facility_2_5_kms=ifelse(df6$Availability_of_government_seed_centres %in% "No ( Nearest facility2-5 kms)", 1, 0)
df6$Availability_of_government_seed_centres_No_Nearest_facility_5_10_kms=ifelse(df6$Availability_of_government_seed_centres %in% "No ( Nearest facility5-10 kms)", 1, 0)
df6$Availability_of_government_seed_centres_No_Nearest_facility_More_than_10_kms=ifelse(df6$Availability_of_government_seed_centres %in% "No ( Nearest facilityMore than 10 kms)", 1, 0)

unique(df6$Availability_of_warehouse_for_Food_Grain_Storage)
df6$Availability_of_warehouse_for_Food_Grain_Storage_Yes=ifelse(df6$Availability_of_warehouse_for_Food_Grain_Storage %in% "Yes", 1, 0)
df6$`Availability_of_warehouse_for_Food_Grain_Storage_No_Nearest_facility_<1_km`=ifelse(df6$Availability_of_warehouse_for_Food_Grain_Storage %in% "No ( Nearest facility< 1 km)", 1, 0)
df6$Availability_of_warehouse_for_Food_Grain_Storage_No_Nearest_facility_1_2_kms=ifelse(df6$Availability_of_warehouse_for_Food_Grain_Storage %in% "No ( Nearest facility1-2 kms)", 1, 0)
df6$Availability_of_warehouse_for_Food_Grain_Storage_No_Nearest_facility_2_5_kms=ifelse(df6$Availability_of_warehouse_for_Food_Grain_Storage %in% "No ( Nearest facility2-5 kms)", 1, 0)
df6$Availability_of_warehouse_for_Food_Grain_Storage_No_Nearest_facility5_10_kms=ifelse(df6$Availability_of_warehouse_for_Food_Grain_Storage %in% "No ( Nearest facility5-10 kms)", 1, 0)
df6$Availability_of_warehouse_for_Food_Grain_Storage_No_Nearest_facilityMore_than_10_kms=ifelse(df6$Availability_of_warehouse_for_Food_Grain_Storage %in% "No ( Nearest facilityMore than 10 kms)", 1, 0)

unique(df6$Does_the_village_have_access_to_Custom_Hiring_Centre_Agri_equipments_)
df6$Does_the_village_have_access_to_Custom_Hiring_Centre_Agri_equipments_Yes=ifelse(df6$Does_the_village_have_access_to_Custom_Hiring_Centre_Agri_equipments_ %in% "Yes", 1, 0)
df6$Does_the_village_have_access_to_Custom_Hiring_Centre_Agri_equipments_No=ifelse(df6$Does_the_village_have_access_to_Custom_Hiring_Centre_Agri_equipments_%in% "No", 1, 0)

unique(df6$Availability_of_fertilizer_shop)
df6$Availability_of_fertilizer_shop_Yes=ifelse(df6$Availability_of_fertilizer_shop %in% "Yes", 1, 0)
df6$`Availability_of_fertilizer_shop_No_Nearest_facility_<1_km`=ifelse(df6$Availability_of_fertilizer_shop %in% "No ( Nearest facility< 1 km)", 1, 0)
df6$Availability_of_fertilizer_shop_No_Nearest_facility_1_2_kms=ifelse(df6$Availability_of_fertilizer_shop %in% "No ( Nearest facility1-2 kms)", 1, 0)
df6$Availability_of_fertilizer_shop_No_Nearest_facility_2_5_kms=ifelse(df6$Availability_of_fertilizer_shop %in% "No ( Nearest facility2-5 kms)", 1, 0)
df6$Availability_of_fertilizer_shop_No_Nearest_facility_5_10_kms=ifelse(df6$Availability_of_fertilizer_shop %in% "No ( Nearest facility5-10 kms)", 1, 0)
df6$Availability_of_fertilizer_shop_No_Nearest_facility_More_than_10_kms=ifelse(df6$Availability_of_fertilizer_shop %in% "No ( Nearest facilityMore than 10 kms)", 1, 0)

unique(df6$Availability_of_Internet_BroadBand_Facility)
df6$Availability_of_Internet_BroadBand_Facility_Yes=ifelse(df6$Availability_of_Internet_BroadBand_Facility %in% "Yes", 1, 0)
df6$Availability_of_Internet_BroadBand_Facility_No=ifelse(df6$Availability_of_Internet_BroadBand_Facility %in% "No", 1, 0)

unique(df6$Availability_of_Markets)
df6$Availability_of_Markets_Regular_market=ifelse(df6$Availability_of_Markets %in% "Regular market", 1, 0)
df6$Availability_of_Markets_Mandis=ifelse(df6$Availability_of_Markets %in% "Mandis", 1, 0)
df6$Availability_of_Markets_Weekly_Haat=ifelse(df6$Availability_of_Markets %in% "Weekly Haat", 1, 0)
df6$`Availability_of_Markets_None_Nearest_facility_<1_km`=ifelse(df6$Availability_of_Markets %in% "None ( Nearest facility1-2 kms)", 1, 0)
df6$Availability_of_Markets_None_Nearest_facility1_2_kms=ifelse(df6$Availability_of_Markets %in% "None ( Nearest facility1-2 kms)", 1, 0)
df6$Availability_of_Markets_None_Nearest_facility2_5_kms=ifelse(df6$Availability_of_Markets %in% "None ( Nearest facility2-5 kms)", 1, 0)
df6$Availability_of_Markets_None_Nearest_facility5_10_kms=ifelse(df6$Availability_of_Markets %in% "None ( Nearest facility5-10 kms)", 1, 0)
df6$Availability_of_Markets_None_Nearest_facility_More_than_10_kms=ifelse(df6$Availability_of_Markets %in% "None ( Nearest facilityMore than 10 kms)", 1, 0)

unique(df6$Availability_of_Business_Correspondent_with_Internet_connectivity) # "Yes" "No"  "_"
df6$Availability_of_Business_Correspondent_with_Internet_connectivity_Yes=ifelse(df6$Availability_of_Business_Correspondent_with_Internet_connectivity %in% "Yes", 1, 0)
df6$Availability_of_Business_Correspondent_with_Internet_connectivity_No=ifelse(df6$Availability_of_Business_Correspondent_with_Internet_connectivity %in% c("No","_"), 1, 0)


unique(df6$Use_of_Solar_Energy_Wind_Energy_for_electrification_of_the_house)

df6$Use_of_Solar_Energy_Wind_Energy_for_electrification_of_the_house_number_of_households=as.numeric(trimws(gsub("Yes Number of Households utilising Solar Energy/Wind Energy :",replacement ="",
                                                                                                      df6$Use_of_Solar_Energy_Wind_Energy_for_electrification_of_the_house)))
df6$Use_of_Solar_Energy_Wind_Energy_for_electrification_of_the_house_number_of_households[is.na(df6$Use_of_Solar_Energy_Wind_Energy_for_electrification_of_the_house_number_of_households) ]= 0


unique(df6$Availability_of_electricity_for_domestic_use_in_Hrs_)
df6$Availability_of_electricity_for_domestic_use_in_Hrs_None=ifelse(df6$Availability_of_electricity_for_domestic_use_in_Hrs_ %in% "None", 1, 0)
df6$Availability_of_electricity_for_domestic_use_in_Hrs_1_4hrs=ifelse(df6$Availability_of_electricity_for_domestic_use_in_Hrs_ %in% "1-4 hrs", 1, 0)
df6$Availability_of_electricity_for_domestic_use_in_Hrs_4_8Hrs=ifelse(df6$Availability_of_electricity_for_domestic_use_in_Hrs_ %in% "4-8 Hrs", 1, 0)
df6$Availability_of_electricity_for_domestic_use_in_Hrs_8_12_Hrs=ifelse(df6$Availability_of_electricity_for_domestic_use_in_Hrs_ %in% "8-12 Hrs", 1, 0)
df6$`Availability_of_electricity_for_domestic_use_in_Hrs_>12_hrs`=ifelse(df6$Availability_of_electricity_for_domestic_use_in_Hrs_ %in% ">12 hrs", 1, 0)

unique(df6$Availability_of_Piped_tap_water)
df6$Availability_of_Piped_tap_water_only_one_habitation_is_covered=ifelse(df6$Availability_of_Piped_tap_water %in% "only one habitation is covered", 1, 0)
df6$`Availability_of_Piped_tap_water_<50%_habitation_covered`= ifelse(df6$Availability_of_Piped_tap_water %in% "<50% habitation covered", 1, 0)
df6$`Availability_of_Piped_tap_water_50to100%_habitations_covered`=ifelse(df6$Availability_of_Piped_tap_water %in% "50 to 100% habitations covered", 1, 0)
df6$`Availability_of_Piped_tap_water_100%_habitations_covered`=ifelse(df6$Availability_of_Piped_tap_water %in% "100% habitations covered", 1, 0)
df6$`Availability_of_Piped_tap_water_None_Nearest_facility_<1km`=ifelse(df6$Availability_of_Piped_tap_water %in% "None ( Nearest facility< 1 km)", 1, 0)
df6$`Availability_of_Piped_tap_water_None_Nearest facility1_2_kms`=ifelse(df6$Availability_of_Piped_tap_water %in% "None ( Nearest facility1-2 kms)", 1, 0)
df6$Availability_of_Piped_tap_water_None_Nearest_facility2_5_kms=ifelse(df6$Availability_of_Piped_tap_water %in% "None ( Nearest facility2-5 kms)", 1, 0)
df6$Availability_of_Piped_tap_water_None_Nearest_facility5_10_kms=ifelse(df6$Availability_of_Piped_tap_water %in% "None ( Nearest facility5-10 kms)", 1, 0)
df6$Availability_of_Piped_tap_water_NoneNone_Nearest_facility_More_than_10_kms=ifelse(df6$Availability_of_Piped_tap_water %in% "None ( Nearest facilityMore than 10 kms)", 1, 0)


# splitting net son area into numeric data by its categories

df6$net_sown_area_temprory= (str_squish(gsub("[^0-9.-]", " ", df6$Net_sown_Area_in_Hectares_If_in_Acres_divide_by_2_47)))

df6=separate(df6,col=net_sown_area_temprory,into=c("Net_sown_Area_in_Hectares_If_in_Acres_divide_by_2_47_Total","Net_sown_Area_in_Hectares_If_in_Acres_divide_by_2_47_kharif",
                                               "Net_sown_Area_in_Hectares_If_in_Acres_divide_by_2_47_Rabi","Net_sown_Area_in_Hectares_If_in_Acres_divide_by_2_47_Other"),
         sep = " ",remove = T)


df6$Net_sown_Area_in_Hectares_If_in_Acres_divide_by_2_47_Total=as.numeric(df6$Net_sown_Area_in_Hectares_If_in_Acres_divide_by_2_47_Total)
df6$Net_sown_Area_in_Hectares_If_in_Acres_divide_by_2_47_kharif=as.numeric(df6$Net_sown_Area_in_Hectares_If_in_Acres_divide_by_2_47_kharif)
df6$Net_sown_Area_in_Hectares_If_in_Acres_divide_by_2_47_Rabi=as.numeric(df6$Net_sown_Area_in_Hectares_If_in_Acres_divide_by_2_47_Rabi)
df6$Net_sown_Area_in_Hectares_If_in_Acres_divide_by_2_47_Other=as.numeric(df6$Net_sown_Area_in_Hectares_If_in_Acres_divide_by_2_47_Other)





######################################################################################################################################
#now calcualte percentage of required data 

df6$percent_of_households_engaged_majorly_in_farm_activity=(df6$No_of_HHs_engaged_majorly_in_Farm_activities/df6$Total_Households)*100

df6$percent_of_households_engaged_majorly_in_Non_farm_activity=(df6$No_of_HHs_engaged_majorly_in_Non_Farm_activities/df6$Total_Households)*100

df6$percent_of_Area_irrigated=(df6$Area_irrigated_in_Hectare_If_in_Acres_divide_by_2_47
                               /df6$Net_sown_Area_in_Hectares_If_in_Acres_divide_by_2_47_Total)*100

df6$percent_of_kuccha_households=(df6$No_of_HHs_with_kuccha_wall_and_kuccha_roof/df6$Total_Households)*100

df6$percent_of_household_with_jandhan_account=(df6$No_of_HHs_having_Jan_Dhan_bank_account/df6$Total_Households)*100

df6$percent_of_BPL_households=(df6$No_of_HHs_having_BPL_ration_cards/df6$Total_Households)*100

df6$percent_of_households_using_clean_energy=(df6$Total_no_of_HHs_using_clean_energy_LPG_Bio_gas_/df6$Total_Households)*100

df6$percent_of_farming_population=(df6$Total_No_of_Farmers/df6$Total_Population)*100

df6$percent_of_farmer_benefitted_under_PMFBY=(df6$No_of_farmers_received_benefits_under_Pradhan_Mantri_Fasal_Bima_Yojana/df6$Total_No_of_Farmers)*100

df6$percent_of_farmers_adding_fertilisers=(df6$No_of_farmers_adding_fertilizer_in_the_soil_as_per_soil_testing_reports/df6$Total_No_of_Farmers)*100

df6$percent_of_households_Using_Solar_Energy_Wind_Energy=(df6$Use_of_Solar_Energy_Wind_Energy_for_electrification_of_the_house_number_of_households/df6$Total_Households)*100



df7=df6[,c(1:5,38,6:16,90,17,91,18,39:44,19,45:50,20,51:52,21,86:89,23,92,22,53:58,36,99,24,93,25,77:85,26,72:76,27,71,100,28,69,70,30,59,60,
           29,94,31,95,32,61:68,33,96,34,97,35,98,37)] #reaarange the columns

df8=df7[,c(1:5,7:44,96:99,45:95,100,6)]



write.csv(df8,"mission_antyodaya_village_with_percentage.csv",row.names = F,na="")





