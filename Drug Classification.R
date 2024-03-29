type.of.drug <- data.frame(Drug.Name = c("6-MAM", "Caffeine","Erythritol",
                                         "etizolam", "Fent <5%","Fentanyl or Analog",
                                         "Heroin","Lactose","Mannitol", 
                                         "No Cuts", "No Library Match","Uncertain Carbohydrate",
                                         "Uncertain Match", "Sildenafil Citrate", "Acetaminophen",
                                         "Ascorbic Acid", "Crack Cocaine","Dimethyl Sulfone",
                                         "Benzocaine","Glucose",  "Inositol",
                                         "MDMA","Microcrystalline Cellulose", "Noscapine",
                                         "Propionanilide","Sucrose","Uncertain Oil",
                                         "Xylazine","Xylitol", "AMB-FUBINACA", 
                                         "Methamphetamine", "Sugar Uncertain", "Heroin Base",
                                         "Benzodiazepine <5%", "Levamisole","Uncertain Salt",
                                         "Water", "3-FPM", "Diphenhydramine",
                                         "No Opioid", "No Cuts Caffeine", 
                                         "No Cuts Fent", "Oxycodone", "Cocaine",
                                         "Calcium Phosphate Dibasic", "4-ANPP", "Phenacetin",
                                         "DXM", "Plain flour", "Quinine",
                                         "Carmellose", "Acetoacetanilide", "Uncertain Oil/Carb/Sugar",
                                         "Taurine", "Cement", "No Cuts Methadone",
                                         "No Cuts Heroin", "Calcium Carbonate", "Ibuprofen",
                                         "Opium", "PEG","Sorbital",
                                         "Creatine", "Morphine","Wax",
                                         "Acetylsalicylic Acid", "Methadone","Tramadol",
                                         "Corn Syrup", "Cellulose", "Procaine",
                                         "Starch", "Lidocaine", "Plaster",
                                         "ACHMINACA", "Boric Acid", "Amphetamine",
                                         "Hydrocodone", "Oxymetholone", "Thiamine",
                                         "Talc", "Calcium Hydroxide","Polyethylene",
                                         "Codeine Sulfate", "doxycycline", "5F-ADB",
                                         "Sodium Bicarbonate", "Benzoquinone","Dextrose",
                                         "Glutamine", "Pantoprazole Sodium", "Codeine",
                                         "Albuterol sulfate","Citric Acid", "Gabapentin",
                                         "Sulfamethoxazole", "Sugar", "Sorbitol",
                                         "MDA", "Ketamine", "Etizolam"
                                         
                                         
),
Classification = c("Opioid", "Stimulant", "Buff",
                   "Benzodiazepine", "Opioid", "Opioid",
                   "Opioid", "Buff", "Buff",
                   "Other or NA", "Other or NA", "Other or NA",
                   "Other or NA", "Other or NA", "Buff",
                   "Buff", "Stimulant", "Buff",
                   "Buff", "Buff", "Buff",
                   "Stimulant", "Buff", "Opioid", 
                   "Buff","Buff", "Other or NA", 
                   "Other or NA", "Buff", "Synthetic Cannabinoid",
                   "Stimulant", "Other or NA", "Opioid",
                   "Benzodiazepine", "Buff", "Other or NA",
                   "Other or NA", "Stimulant", "Other or NA",
                   "Other or NA", "Other or NA", 
                   "Other or NA", "Opioid", "Stimulant",
                   "Buff", "Precursor", "Buff",
                   "Other or NA", "Buff", "Other or NA",
                   "Other or NA", "Other or NA", "Other or NA",
                   "Other or NA", "Other or NA", "Opioid",
                   "Opioid", "Buff", "Buff",
                   "Opioid", "Buff", "Buff",
                   "Other or NA", "Opioid", "Other or NA",
                   "Other or NA", "Opioid", "Opioid",
                   "Buff", "Buff", "Buff", 
                   "Buff", "Buff", "Other or NA",
                   "Synthetic Cannabinoid", "Other or NA", "Stimulant",
                   "Opioid", "Other or NA", "Other or NA",
                   "Other or NA", "Other or NA", "Other or NA",
                   "Opioid", "Other or NA", "Synthetic Cannabinoid",
                   "Buff", "Other or NA", "Buff",
                   "Buff", "Other or NA", "Opioid",
                   "Other or NA", "Buff", "Other or NA",
                   "Other or NA", "Buff", "Buff",
                   "Stimulant", "Dissociative", "Benzodiazepine")
)
type.of.drug <- type.of.drug[!grepl("No Cuts", type.of.drug$Drug.Name),]
drug2 <- data.frame(Drug.Name = c("NPP", "Polystyrene",                 
                                  "Piperidone", "Metamizole",                  
                                  "Etonitazene", "Trazodone", 
                                  "W-19", "Flubromazepam",           
                                  "Alprazolam", "Sodium Citrate",
                                  "Sodium Isoascorbate", "Vitamin C" ),
                    Classification = c("Other or NA", "Other or NA",
                                       "Precursor", "Buff",
                                       "Opioid", "Other or NA",
                                       "Other or NA", "Benzodiazepine",
                                       "Benzodiazepine", "Buff",
                                       "Buff", "Buff"))
ee <- data.frame(Drug.Name = c("4-HO-MiPT", "Sildenafil",                     
 "Uncertain Mineral","GHB (Wet)",                      
 "Hydromorphone",                  "DMT",                            
 "TFMPP"  ,                         "Melatonin",                      
 "Quetiapine",                      "Carfentanil",                    
 "Uncertain Wax",                   "4-HO-MET",                       
 "Tadalafil" ,                      "Ephedrine",                      
 "Oxandrolone",                     "Dicalcium Phosphate",            
 "Methylamine",                     "Nandrolone Decanoate",           
 "Trenbolone enanthate",            "Testosterone Cypionate",         
 "Testosterone Isocaproate",        "Diazepam",                       
 "Clonazepam",                      "Flualprazolam",                  
 "Zopiclone",                       "Testosterone Enanthate",         
 "25I-NBOMe",                       "Pyrazolam",                      
 "Calcium Stearate",                "Chlorodehydromethyltestosterone",
 "Raloxifene",                      "THC",                            
 "N-Ethylhexedrone",                "Hesperidin",                     
 "4-Anilinopiperidine",             "Magnesium Sulfate",              
 "Modafinil",                       "Methylphenidate",                
 "Gliclazide",                      "2C-B",                           
 "GBL",                             "Phenethylamine",                 
 "Adinazolam",                      "2C-I",                           
 "5-MeO-DiPT",                      "Lorazepam",                      
 "Aspirin",                         "Stanzolol",                      
 "Mescaline",                       "Mesterolone",                    
 "Propylene Glycol",                "Ceftazidime",                    
 "Carisoprodol",     "Monosodium Glutamate",           
 "GHB (Dry)" ,      "Tetracaine",                     
 "Isoleucine"               ,       "Methionine",                     
 "Cannabidiol"             ,        "Amphetamine Sulfate",            
 "Nicotinamide"           ,         "Calcium Phosphate",              
 "Methocarbamol"         ,          "Safrole",                        
 "methandrostenolone"   ,           "2-FDCK",                         
 "Bromazolam"         ,             "Somatropin",                     
 "Stearic acid",                    "Menthol",                        
 "3-MeO-PCP",                       "Clomiphine",                     
 "Fumaric Acid" ,                 "BZP",                            
 "5-MeO-MiPT",                      "Lamotrigine",                    
 "4-Chloroethcathinone" ,          "5-MeO-DMT",                      
 "DMT Base",                        "Temazepam",                      
 "THC-A",                           "Leucine",                        
 "N-ethylheptedrone"    ,           "Ibogaine",                       
 "Piperidine"    ,                  "Ethanol",                        
 "Phenylalanine",                   "Amlodipine Besylate",            
 "Eutylone",                        "N-ethylhexedrone",               
 "Lisdexamfetamine",                "Glucosamine Sulfate",            
 "Fructose",                       "N-Ethylpentylone",               
 "4-FA",                            "Olanzapine",                     
 "Sodium Sulfate",                  "Pseudoephedrine",                
 "Phenibut", "Exemestane",                     
"N-Ethylnorketamine","Dimethindene","Nimetazepam",
"N-Ethylheptedrone",   "Tianeptine",          "Sodium isoascorbate",
"para-Fluorofentanyl", "Isotonitazene",
"Methcathinone", "Ephenidine",
"Metonitazene", "ADB-PINACA", "Opioids no Fentanyl/Fentanyl Analogues"),
Classification = c("Psychedelic", "Other or NA",
  "Other or NA", "Depressant",
  "Opioid", "Psychedelic",
  "Stimulant", "Other or NA",
  "Other or NA", "Opioid",
  "Other or NA", "Psychedelic",
  "Other or NA", "Stimulant",
  "Steroid", "Buff",
  "Precursor", "Steroid",
  "Steroid", "Steroid",
  "Steroid", "Benzodiazepine",
  "Benzodiazepine", "Benzodiazepine",
  "Depressant", "Steroid",
  "Psychedelic", "Benzodiazepine",
  "Buff", "Steroid",
  "Buff", "Cannabinoid",
  "Stimulant", "Buff",
  "Precursor", "Buff",
  "Stimulant", "Stimulant",
  "Buff", "Psychedelic",
  "Depressant", "Buff", 
  "Benzodiazepine", "Psychedelic",
  "Psychedelic", "Benzodiazepine",
  "Buff", "Steroid",
  "Psychedelic", "Steroid",
  "Buff","Other or NA",
  "Other or NA", "Buff",
  "Depressant", "Buff",
  "Buff", "Other or NA",
  "Cannabinoid", "Stimulant",
  "Other or NA", "Buff",
  "Other or NA", "Other or NA",
  "Steroid", "Dissociative",
  "Benzodiazepine", "Other or NA",
  "Buff", "Other or NA",
  "Dissociative", "Other or NA",
  "Other or NA", "Stimulant",
  "Psychedelic", "Other or NA",
  "Stimulant", "Psychedelic",
  "Psychedelic", "Benzodiazepine",
  "Cannabinoid", "Other or NA",
  "Stimulant", "Other or NA",
  "Precursor", "Depressant",
  "Buff", "Other or NA",
  "Stimulant", "Stimulant",
  "Stimulant", "Buff",
  "Other or NA", "Stimulant",
  "Stimulant", "Other or NA",
  "Buff", "Stimulant",
  "Depressant", "Other or NA",
  "Dissociative", "Other or NA",
  "Benzodiazepine",
  "Stimulant", "Other or NA", "Buff",
  "Opioid", "Opioid",
  "Stimulant", "Dissociative",
  "Opioid","Synthetic Cannabinoid", 
  "Opioid"))                    

type.of.drug <- rbind(type.of.drug, drug2, ee)
rm(drug2, ee)
type.of.drug2 <- type.of.drug %>%
  mutate(Drug.Name = paste("No Cuts",Drug.Name, sep = " "))

type.of.drug <- rbind(type.of.drug, type.of.drug2)
type.of.drug$Drug.Name <- gsub("(No Cuts) ", "\\1\n", type.of.drug$Drug.Name)

rownames(type.of.drug) <- NULL
rm(type.of.drug2)
type.of.drug$Drug.Name <- stringr::str_to_title(type.of.drug$Drug.Name)

