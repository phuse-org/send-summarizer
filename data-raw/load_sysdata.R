#!/usr/bin/env Rscript

# This code is for creating
# -----------------------------------------------------------------------------
# Date                     Programmer
#----------   --------------------------------------------------------------
# Feb-03-2025    Md Yousuf Ali (MdYousuf.Ali@fda.hhs.gov)


MITESTCDlist <- list('LIVER' = c('LIVER'),
                     'KIDNEY' = c('KIDNEY'),
                     'HEMATOPOIETIC' = c('BONE MARROW', 'SPLEEN', 'THYMUS','LYMPH NODE, MESENTERIC', 'LYMPH NODE, MANDIBULAR'),
                     'ENDOCRINE' = c('GLAND, THYROID', 'GLAND, ADRENAL', 'GLAND, PITUITARY',
                                                        'GLAND, PARATHYROID', 'PANCREAS'),
                     'REPRODUCTIVE' = c('CERVIX','EPIDIDYMIS','GLAND, PROSTATE','GLAND, MAMMARY',
                                                      'OVARY','PROSTATE','TESTIS','UTERUS','VAGINA'))
OMTESTCDlist <- MITESTCDlist
organTESTCDlist <- list('LIVER' = c('SERUM | ALT',
                                    'SERUM | AST',
                                    'SERUM | ALP',
                                    'SERUM | GGT',
                                    'SERUM | BILI',
                                    'SERUM | ALB'),
                        'KIDNEY' = c('SERUM | CREAT',
                                     'SERUM | UREAN',
                                     'SERUM | ALB',
                                     'SERUM | CL',
                                     'SERUM | K',
                                     'SERUM | PHOS',
                                     'SERUM | SODIUM',
                                     'URINE | CL',
                                     'URINE | K',
                                     'URINE | SODIUM',
                                     'URINE | GLUC',
                                     'URINE | SPGRAV',
                                     'URINE | VOLUME',
                                     'URINE | PROT',
                                     'URINE | UROBIL'),
                        'HEMATOPOIETIC' = c( 'WHOLE BLOOD | RBC',
                                             'WHOLE BLOOD | HCT',
                                             'WHOLE BLOOD | MCHC',
                                             'WHOLE BLOOD | MCH',
                                             'WHOLE BLOOD | MCV',
                                             'WHOLE BLOOD | RDW',
                                             'WHOLE BLOOD | WBC',
                                             'WHOLE BLOOD | MONO',
                                             'WHOLE BLOOD | BASO',
                                             'WHOLE BLOOD | EOS',
                                             'WHOLE BLOOD | LYM',
                                             'WHOLE BLOOD | PLAT',
                                             'WHOLE BLOOD | MPV'),
                        'ENDOCRINE' = c('URINE | CL',
                                        'URINE | K',
                                        'URINE | SODIUM',
                                        'URINE | GLUC',
                                        'URINE | SPGRAV',
                                        'URINE | VOLUME',
                                        'URINE | PROT'),
                        'REPRODUCTIVE' = c('SERUM | GNRH',
                                           'SERUM | LH',
                                           'SERUM | FSH',
                                           'SERUM | DHT',
                                           'SERUM | DOXMTST',
                                           'SERUM | FAI',
                                           'SERUM | MTESTOS',
                                           'SERUM | NANDRLN',
                                           'SERUM | TESTOS',
                                           'SERUM | TESTOSBA',
                                           'SERUM | TESTOSFR',
                                           'SERUM | TESTOSWB',
                                           'SERUM | TSTFTSTT',
                                           'SERUM | TSTFWTST',
                                           'SERUM | TST4OH',
                                           'SERUM | ESTROGEN'))
doseRanks <- c('Vehicle', 'LD', 'MD', 'HD')






usethis::use_data(OMTESTCDlist,
                  MITESTCDlist,
                  organTESTCDlist,
                  doseRanks,
                  internal = TRUE,
                  overwrite = TRUE)
