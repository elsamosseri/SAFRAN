extensions [ gis csv matrix ]

globals [
  environment ; GIS - vector layer showing the spatial extent of the model
  ; here stands for 9 115 km2
  habitats ; GIS - vector layer showing major marine habitats of the MPA
  bathy ; GIS - raster layer showing the bathymetry in the perimeter of the MPA
  coastline ; GIS - vector layer showing the coastline
  lakes-lagoons ; GIS - vector layer showing the lakes and lagoons
  white-gorgonian ; GIS - vector layer showing the biomass density of "Eunicella Singularis"
  ; site-specific data produced during the RocConnect project completed in 2016 and coordinated by Katell Guizien
  ; http://isidoredd.documentation.developpement-durable.gouv.fr/document.xsp?id=Temis-0084332
  reefs-villages ; GIS - vector layer showing the location of artificial reefs villages
  milles ; GIS - raster layer showing the distance to the coast
  buffer ; GIS - vector layer showing the distance to the coast
  nat-rich ; GIS - vector layer corresponding to a map from the MPA management plan that graduates important natural areas
  marine-reserve ; GIS - vector layer indicating the boundaries of the existing FPA (Cerbère-Banyuls natural Marine Reserve)
  windfarm ; GIS - vector layer created from a map used by the management team of the MPA
  ; to initiate debates on the location of a windfarm -
  ; the perimeter under study in real life has changed since SAFRAN's project completion
  wind-anchors ; GIS - vector layer showing the possible location of wind-anchors for floating wind-turbines
  wind-turbines ; GIS - vector layer showing the possible location of floating wind-turbines
  diving ; GIS - vector layer showing the most popular diving sites of the MPA
  y ; on-going simulation year
  diving-patches-init ; number of patches open to diving at base year
  diving-patches-init-sand ; number of patches open to diving at base year in the sand ecosystem
  diving-patches-init-mud ; number of patches open to diving at base year in the mud ecosystem
  diving-patches-init-rock ; number of patches open to diving at base year in the rock ecosystem
  diving-patches-init-coral ; number of patches open to diving at base year in the posidonia ecosystem
  diving-patches-init-posi ; number of patches open to diving at base year in the coralligenous ecosystem
  tot-divers/diving-patches/year-init ; total number of divers associated with diving sites at base year
  tot-divers/diving-patches/year ; total number of divers associated with patches open to diving the year the simulation ends
  wind-turbines-patches-init ; number of patches concerned with wind-turbines at base year
  wind-turbines-patches ; number of patches concerned with wind-turbines the year the simulation ends
  tot-passenger/year ; number of passengers having sea-visited the windwarm the year the simulation ends
  new-reefs-fishing ; string - the way of fishing over new artifical reefs can be "limited" or "extended" depending on the scenario
]

patches-own [
  bat ; batymetry of the patch
  bioc ; biocenosis of the patch
  gorg ; value of the biomass density of "Eunicella Singularis" of the patch
  reefs ; presence of reefs on the patch: "true" or "false"
  nat-rich-level ; value of important natural areas on the patch
  mil ; distance to the coast of the patch
  active ; only patches associated with ecosystem's biomass data are considered active
  ; variables used to proceed to calibration -> compute-data
  data ; data files for each ecosystem
  data-copy ; copy of the original data files for each ecosystem
  B ; list of the biomass of each species for the ecosystem of the patch
  B-copy ; copy of the list of the biomass of each species for the ecosystem of the patch
  Q/B ; list of the consumption of each species per unit of biomass for the ecosystem of the patch
  trawls-share lamparos-share tuna-seiner-share other-artisanal-fisheries-share ; distribution of each species catches among the different types of fisheries
  trawls-ratio lamparos-ratio tuna-seiner-ratio other-artisanal-fisheries-ratio ; share of each species in the catches of each type of fishery
  trawls-fished-biomass lamparos-fished-biomass tuna-seiner-fished-biomass other-artisanal-fisheries-fished-biomass ; fished biomass of each species for each type of fishery
  fished-biomass-init ; matrix of the fished biomass of each species for each type of fishery
  landing-ratio ; share of each species in total catches (all types of fishery)
  C ; list of the fished biomass of each species for the ecosystem of the patch (all types of fishery)
  C-gross ; list of the total biomass fished in the ecosystem (all types of fishery)
  C-copy ; copy of the list of the fished biomass of each species for the ecosystem of the patch (all types of fishery)
  Q ; list of the gross consumption of each species for the ecosystem of the patch
  Q-2 ; list of the gross predation mortality of each species for the ecosystem of the patch
  Q-2/B ; list of the gross predation mortality of each species per unit of biomass for the ecosystem of the patch
  diet-matrix ; matrix of the detailed gross diet of each species for the ecosystem of the patch
  diet-matrix-2 ; matrix of the detailed gross predation of each species for the ecosystem of the patch
  Q/B-matrix ; matrix of "Q/B" of length "B"
  Q-2/B-matrix ; matrix of "Q-2/B" of length "B"
  Q-matrix ; matrix of "Q" of length "B"
  Q-2-matrix ; matrix of "Q-2" of length "B"
  ; variables used to proceed to yearly species biomass calculation using a combination of two types of controlling factors -> compute-B
  B-matrix ; matrix of "B" of length "B"
  gross-diet ; list of the gross diet of each species for the ecosystem of the patch
  gross-diet-copy ; copy of "gross-diet"
  gross-predation ; list of the gross predation of each species for the ecosystem of the patch
  gross-predation-copy ; copy "gross-predation-copy"
  gross-diet-detail ; matrix of the detailed gross diet of each species for the ecosystem of the patch
  gross-predation-detail ; matrix of the detailed gross predation of each species for the ecosystem of the patch
  diet-ratio ; matrix of the diet ratio of each species on another species for the ecosystem of the patch
  predation-ratio ; matrix of the predation ratio of each species on another species for the ecosystem of the patch
  B-prey ; list of the biomass of each species for the ecosystem of the patch
  ; after the potential influence on prey species on each other species
  ; bottom-up control, positive feedback
  B-preda ; list of the biomass of each species for the ecosystem of the patch
  ; after the potential influence on predatory species on each other species
  ; top-down control, negative feedback
  B-preda-copy ; copy of "B-preda-copy"
  P-1 ; residual known preys of group of species
  P-2 ; residual known predators of group of species
  P-2-copy ; copy of "P-2"
  B-2018 ; list of the biomass of each species for the ecosystem of the patch at base year
  B-end ; list of the biomass of each species for the ecosystem of the patch after the combination of the two types of controlling factors
  C-2018 ; list of the fished biomass of each species for the ecosystem of the patch (all types of fishery) at base year
  C-end ; list of the fished biomass of each species for the ecosystem of the patch (all types of fishery) after checking availability of targeted species
  fished-biomass-2018 ; matrix of fished biomass of each species for each type of fishery in 2018
  ; variables used to introduce the assumption on phytoplankton biomass change -> modify-B
  phyto-init ; value of phytoplankton biomass at base year
  phyto-year ; value of phytoplankton biomass each year
  ; variables used to introduce the hypothesis of variation in fishing effort by type of fishery -> to modify-C
  trawls-new-demand lamparos-new-demand tuna-seiner-new-demand other-artisanal-fisheries-new-demand ; new aggregate demand for fishing by type of fishery in case of a change in fishing effort (Interface)
  ; variables used to check there's enough species biomass to be fished -> check-fishing-poss
  fished-detail  ; matrix of fished biomass of each species for each type of fishery after checking there's enough species biomass to be fished
  predation-detail ; matrix of the detailed gross predation of each species
  checked-fished-detail ; matrix of fished biomass of each species for each type of fishery after checking there's enough species biomass to be fished
  ; variables used to develop-FPA
  no-go-take ; if "true", fishing and diving are prohibited in the patch area
  reserve ; if = 56, the patch corresponds to the Cerbère-Banyuls Marine Nature Reserve
  turbines-area ; the patch area can be designated "général" or "acceptable"
  turbines ; if "true", there is one or more turbines in the patch area
  open-to-diving ; if "true", patch is open to diving, if "false", patch is concerned with FPA and not open to diving
  ; variables used to scale biomass density of reefs villages -> scale-reefs
  dens-init ; estimated initial density of an artificial reef village
  reefs-dens-max-detail ; copy of initial values before scaling
  reefs-dens-max ; copy of initial values before scaling
  reefs-dens ; biomass density of an artificial reef village after scaling data
  dens-year ; density of an artificial reef village each year
  ; variables used to act on reefs villages
  reefs-state ; reefs villages can be "old" or "new" based on real-life data -
  ; if "old", the villages can be densified (densi-reefs) and/or prohibited to fishing (forbid-fishing-reefs)
  ; if "new", the villages can be extended (grow-reefs)
  reefs-cult ; reefs villages can be "true" or "false" based on real-life data -
  ; if "true", reefs villages may have a cultural fuction (dive-reefs)
  reefs-repop ; reefs villages can be "true" or "false" based on real-life data -
  ; if "true", reefs villages may be temporarily repopulated with heritage (repop-reefs-S1) or commercial (repop-reefs-S2) species
  divers/diving-patch/year ; number of divers associated with each patch open to diving -
  ; this parameter changes when recreational divers are associated with cultural artificial
  organization-number ; number of organization concerned with cultural reefs villages
  reefs-public ; used to differentiate the year of opening to diving of new cultural reefs villages
]

to setup

  clear-all
  set y 2000
  output-print y
  setup-GIS
  setup-data
  set phyto-change? false
  set fishing-change? false
  set fully-protected-areas? false
  reset-ticks

end

to go

  clear-output
  set y ( y + 1 )
  output-print y

  if scenario = "no"
  [
    ask patches with [ active ]
    [
      if phyto-change?
      [ modify-B ]
      if fishing-change?
      [ modify-C ]
      compute-B
    ]
  ]

    if scenario = "S1"
  [
    set phyto-change? true ; ignore this line to run S1 no CC
    set phyto-2100 ( - 10 ) ; ignore this line to run S1 no CC
    set fully-protected-areas? true
    set zoning "FPA -> 30%"
    ask patches with [ active ] ; ignore this line to run S1 no CC
    [ modify-B ] ; ignore this line to run S1 no CC
    ask patches with [ nat-rich-level > 0 ]
    [ develop-FPA ]
    ask patches with [ active ]
    [ compute-B
      if reefs-state = "old"
      [ forbid-fishing-reefs ]
      if reefs-repop = true
      [ repop-reefs-S1 ]
    ]
  ]

    if scenario = "S2"
  [
;    set phyto-change? true ; remove semicolon to run S2 + CC
;    set phyto-2100 (- 10) ; remove semicolon to run S2 + CC
    set fishing-change? true
    set trawls-var 5
    set lamparos-var 5
    set tuna-seiner-var 5
    set other-artisanal-fisheries-var 5
;    ask patches with [ active ] ; remove semicolon to run S2 + CC
;    [ modify-B ] ; remove semicolon to run S2 + CC
    set fully-protected-areas? true
    set zoning "FPA -> 2%"
    ask patches with [ active ]
    [ modify-C ]
    ask patches with [ nat-rich-level > 0 ]
    [ develop-FPA ]
    set new-reefs-fishing "limited"
    ask patches with [ active ]
    [ compute-B
      if reefs-state = "old"
      [ densi-reefs ]
      if reefs-repop = true
      [ repop-reefs-S2 ]
      if reefs-state = "new"
      [ grow-reefs ]
    ]
  ]

    if scenario = "S3"
  [
    set phyto-change? true ; ignore this line to run S3 no CC
    set phyto-2100 (- 10) ; ignore this line to run S3 no CC
    set fully-protected-areas? true
    set zoning "FPA -> 10%"
    ask patches with [ active ] ; ignore this line to run S3 no CC
    [ modify-B ] ; ignore this line to run S3 no CC
    ask patches with [ nat-rich-level > 0 ]
    [ develop-FPA ]
    set new-reefs-fishing "extended"
    ask patches with [ active ]
    [ compute-B
      if reefs-state = "old"
      [ densi-reefs ]
      if reefs-state = "new"
      [ grow-reefs ]
      if reefs-cult = true
      [ dive-reefs ]
      if turbines = true
      [ sea-visit ]
    ]
  ]

  if y >= 2018
  [ launch-plots ]

  tick

end

to go-2050

  no-display
  repeat 50 [ go ]
  display
  print-results

end

to setup-data

  import-data
  ask patches with [ data != 0 ] [ compute-data ]
  ask patches with [ data != 0 ] [ diffuse-data ]
  ask patches with [ reefs = true ] [ scale-reefs ]
  ask patches with [ active ] [ restrict-fishing ]
  setup-acti

end

; import data files for each ecosystem at one representative patch level -
; data will be computed at that patch level then extended to similar patches
to import-data

   ask one-of patches with [ bioc = "sand" ]
  [ file-open "sand.csv"
    set data matrix:from-row-list csv:from-file "sand.csv"
    set data-copy data ]
    ask one-of patches with [ bioc = "mud" ]
  [ file-open "sand.csv"
    set data matrix:from-row-list csv:from-file "sand.csv"
    set data-copy data ]
  ask one-of patches with [ bioc = "rock" ]
  [ file-open "rock.csv"
    set data matrix:from-row-list csv:from-file "rock.csv"
    set data-copy data ]
  ask one-of patches with [ bioc = "posi" ]
  [ file-open "posi.csv"
    set data matrix:from-row-list csv:from-file "posi.csv"
    set data-copy data ]
  ask one-of patches with [ bioc = "coral" ]
  [ file-open "coral.csv"
    set data matrix:from-row-list csv:from-file "coral.csv"
    set data-copy data ]

end

; input data formatting
to compute-data

;to help the generalization of the following operations
;regardless of the size of the input data table
;i.e. the number of interacting species in the ecosystem
  let row-number length matrix:get-column data 0
  let column-number length matrix:get-row data 0
;to scale the data according to the number of patches and the corresponding spatial coverage
;here according to the specifications of the project, each patch corresponds to a surface of 0,25 km2
;when "world" model settings of 95 * 95 _ location of origin "center" _ world doesn't wrap
  let p-surf-km2 0.25
  let p-surf-matrix matrix:make-constant row-number column-number p-surf-km2
  set data matrix:times-element-wise data p-surf-matrix
  set data-copy data
;to identify the biomass of each species (B) and the consumption of each species per unit of biomass (Q/B)
  set B matrix:get-row data (row-number - 2)
;to introduce the specific data of K. Guizien & L. Bramanti on "Eunicella Singularis"
  if bioc = "rock" and gorg > 0 [ set B replace-item 6 B ( gorg * 1000 ) ]
  set Q/B matrix:get-row data ( row-number - 1 )
;to compute fishing
;to calculate the fished biomass of each species (C) knowing
;the total biomass fished in the ecosystem (C-gross) and
;the share of each species in total catches (landing-ratio)
  set landing-ratio matrix:get-column data ( column-number - 5 )
  repeat 2 [ set landing-ratio but-last landing-ratio ]
  set C-gross n-values length landing-ratio [ item ( length B - 5 ) B ]
  set C []
  ( foreach landing-ratio C-gross [ [ ?1 ?2 ] -> set C lput ( ?1 * ?2 ) C ] )
;to compute specific fishing
;to calculate the fished biomass of each species for each type of fishery (x-fished-biomass) knowing
;the fished biomass of each species (C)
;the distribution of each species catches among the different types of fisheries (x-share)
  set trawls-share matrix:get-column data ( column-number - 4 )
  repeat 2 [ set trawls-share but-last trawls-share ]
  set lamparos-share matrix:get-column data ( column-number - 3 )
  repeat 2 [ set lamparos-share but-last lamparos-share ]
  set tuna-seiner-share matrix:get-column data ( column-number - 2 )
  repeat 2 [ set tuna-seiner-share but-last tuna-seiner-share ]
  set other-artisanal-fisheries-share matrix:get-column data ( column-number - 1 )
  repeat 2 [ set other-artisanal-fisheries-share but-last other-artisanal-fisheries-share ]
  set trawls-fished-biomass []
  set lamparos-fished-biomass []
  set tuna-seiner-fished-biomass []
  set other-artisanal-fisheries-fished-biomass []
  ( foreach trawls-share lamparos-share tuna-seiner-share other-artisanal-fisheries-share C [ [ ?1 ?2 ?3 ?4 ?5 ] ->
    set trawls-fished-biomass lput ( ?1 * ?5 ) trawls-fished-biomass
    set lamparos-fished-biomass lput ( ?2 * ?5 ) lamparos-fished-biomass
    set tuna-seiner-fished-biomass lput ( ?3 * ?5 ) tuna-seiner-fished-biomass
    set other-artisanal-fisheries-fished-biomass lput ( ?4 * ?5 ) other-artisanal-fisheries-fished-biomass ] )
;to calculate the share of each species in the catches of each type of fishery (x-ratio)
;knowing the biomass caught of each species by each type of fishery (x-fished-biomass)
  let peche_t0 []
  set peche_t0 ( list trawls-fished-biomass lamparos-fished-biomass tuna-seiner-fished-biomass other-artisanal-fisheries-fished-biomass )
  set fished-biomass-init matrix:submatrix matrix:transpose matrix:from-row-list peche_t0 0 0 ( length trawls-fished-biomass - 1 ) 4
  set trawls-ratio divide trawls-fished-biomass ( n-values length trawls-fished-biomass [ sum trawls-fished-biomass ] )
  set lamparos-ratio divide lamparos-fished-biomass ( n-values length lamparos-fished-biomass [ sum lamparos-fished-biomass ] )
  set tuna-seiner-ratio divide tuna-seiner-fished-biomass ( n-values length tuna-seiner-fished-biomass [ sum tuna-seiner-fished-biomass ] )
  set other-artisanal-fisheries-ratio divide other-artisanal-fisheries-fished-biomass ( n-values length other-artisanal-fisheries-fished-biomass [ sum other-artisanal-fisheries-fished-biomass ] )
  set trawls-ratio lput 1 lput ( sum trawls-fished-biomass ) trawls-ratio
  set lamparos-ratio lput 1 lput ( sum lamparos-fished-biomass ) lamparos-ratio
  set tuna-seiner-ratio lput 1 lput ( sum tuna-seiner-fished-biomass ) tuna-seiner-ratio
  set other-artisanal-fisheries-ratio lput 1 lput ( sum other-artisanal-fisheries-fished-biomass ) other-artisanal-fisheries-ratio
;to replace landing-shares by landing-ratios
  matrix:set-column data ( column-number - 4 ) trawls-ratio
  matrix:set-column data ( column-number - 3 ) lamparos-ratio
  matrix:set-column data ( column-number - 2 ) tuna-seiner-ratio
  matrix:set-column data ( column-number - 1 ) other-artisanal-fisheries-ratio
;to re-size landing-ratios
  repeat 2 [ set trawls-ratio but-last trawls-ratio ]
  repeat 2 [ set lamparos-ratio but-last lamparos-ratio ]
  repeat 2 [ set tuna-seiner-ratio but-last tuna-seiner-ratio ]
  repeat 2 [ set other-artisanal-fisheries-ratio but-last other-artisanal-fisheries-ratio ]
;to extract aggregate landings
  let data-inter-1 matrix:submatrix data 0 0 row-number ( column-number - 5 )
  let data-inter-2 matrix:submatrix data 0 ( column-number - 4 ) row-number column-number
  let data-inter-3 matrix:make-constant row-number ( column-number - 1 ) 0
  let range-data-1 ( range ( column-number - 5 ) )
  set data-inter-3 switch-columns range-data-1 range-data-1 data-inter-1 data-inter-3
  set range-data-1 ( range 4 )
  let range-data-2 ( range ( column-number - 5 ) ( column-number - 1 ) )
  set data-inter-3 switch-columns range-data-1 range-data-2 data-inter-2 data-inter-3
;new dataset containing only diet or landing ratios
  set data data-inter-3
;to update B, Q/B, C
  set B matrix:get-row data ( row-number - 2 )
  if bioc = "rock" and gorg > 0 [ set B replace-item 6 B ( gorg * 1000 ) ]
  set Q/B matrix:get-row data ( row-number - 1 )
  set C but-last C
;to compute consumption of each species (Q)
  set Q []
  ( foreach B Q/B [ [ ?1 ?2 ] -> set Q lput ( ?1 * ?2 ) Q ] )
  set Q-matrix matrix:from-row-list n-values ( length B ) [ Q ]
  set Q/B-matrix matrix:from-row-list n-values ( length B ) [ Q/B ]
;to compute diet
  set diet-ratio matrix:submatrix data 0 0 ( length B ) ( length B )
  set diet-matrix matrix:times-element-wise diet-ratio Q-matrix
;to compute predation
  set diet-matrix-2 matrix:transpose diet-matrix
;to compute predation mortality of each species Q-2
  set Q-2 map [ i -> sum i ] ( matrix:to-column-list diet-matrix-2 )
;to compute Q-2/B
  set Q-2/B []
  ( foreach Q-2 B [ [ ?1 ?2 ] -> ifelse ?2 = 0
    [ set Q-2/B lput 0 Q-2/B ]
    [ set Q-2/B lput ( ?1 / ?2 ) Q-2/B ] ] )
  set Q-2/B-matrix matrix:from-row-list n-values ( length B ) [ Q-2/B ]
;to compute predation
  set Q-2-matrix matrix:from-row-list n-values ( length B ) [ Q-2 ]
  set predation-ratio matrix:from-row-list ( map divide matrix:to-row-list diet-matrix-2 matrix:to-row-list Q-2-matrix )
;to save initial values
  set B-copy B
  set C-copy C

end

; to extend formatted input data to other similar patches ie. standing for the same ecosystem
to diffuse-data

  ask patches with [ bioc = [ bioc ] of myself ] [
    set data [ data ] of myself
    set B [ B ] of myself
    set Q/B [ Q/B ] of myself
    set C [ C ] of myself
    set trawls-ratio [ trawls-ratio ] of myself
    set lamparos-ratio [ lamparos-ratio ] of myself
    set tuna-seiner-ratio [ tuna-seiner-ratio ] of myself
    set other-artisanal-fisheries-ratio [ other-artisanal-fisheries-ratio ] of myself
    set Q [ Q ] of myself
    set Q-matrix [ Q-matrix ] of myself
    set Q/B-matrix [ Q/B-matrix ] of myself
    set diet-ratio [ diet-ratio ] of myself
    set diet-matrix [ diet-matrix ] of myself
    set Q-2 [ Q-2 ] of myself
    set Q-2/B [ Q-2/B ] of myself
    set Q-2/B-matrix [ Q-2/B-matrix ] of myself
    set Q-2-matrix [ Q-2-matrix ] of myself
    set predation-ratio [ predation-ratio ] of myself
    set fished-biomass-init [ fished-biomass-init ] of myself
    set B-copy [ B-copy ] of myself
    set C-copy [ C-copy ] of myself
  ]

end

; to scale biomass density of reefs villages
to scale-reefs

;to save values before scaling
  set reefs-dens-max-detail B
  set reefs-dens-max sum B
;estimated initial density of an artificial reef village of the MPA
  set dens-init 12
  let items []
  set items range length B
  foreach items [ [ ? ] -> set B replace-item ? B ( item ? B * dens-init / 100 ) ]
  adjust-fishing
  set B-copy B
  set C-copy C
  set reefs-dens precision ( sum B / reefs-dens-max * 100 ) 0

end

; to apply practical and legal concerns linked with trawls and artisanal fisheries
; artisanal fisheries work mostly near the coast up to a maximum distance of 6 nautical miles and a maximum depth of -200 meters
; trawls are prohibited between 0 and 3 nautical miles (2013 Trawl Management Plan)
to restrict-fishing

; other-artisanal-fisheries
  if bat < -200 and mil > 6
  [ set B replace-item ( length B - 1 ) B 0
    adjust-fishing
    set B-copy B
    set C-copy C ]
; trawls
  if mil < 3
  [ set B replace-item ( length B - 4 ) B 0
    adjust-fishing
    set B-copy B
    set C-copy C ]

end

; to scale fished biomass density due to practical and legal concerns linked with trawls and artisanal fisheries
to adjust-fishing

  let fishing-ratio matrix:from-column-list ( list trawls-ratio lamparos-ratio tuna-seiner-ratio other-artisanal-fisheries-ratio )
  let trawls-B n-values length trawls-ratio [ item ( length B - 4 ) B ]
  let lamparos-B n-values length lamparos-ratio [ item ( length B - 3 ) B ]
  let tuna-seiner-B n-values length tuna-seiner-ratio [ item ( length B - 2 ) B ]
  let other-artisanal-fisheries-B n-values length other-artisanal-fisheries-ratio [ item ( length B - 1 ) B ]
  let fishing-tot matrix:from-column-list ( list trawls-B lamparos-B tuna-seiner-B other-artisanal-fisheries-B )
  set fished-detail matrix:times-element-wise fishing-ratio fishing-tot
  set fished-biomass-init fished-detail
  set C []
  set C map [ i -> sum i ] ( matrix:to-row-list fished-detail )

end

; to setup data concerning diving and windfarm locations, and diving attendance
to setup-acti

;diving sites
  set diving-patches-init count patches with [ open-to-diving = true ]
  set diving-patches-init-sand count patches with [ open-to-diving = true and bioc = "sand" ]
  set diving-patches-init-mud count patches with [ open-to-diving = true and bioc = "mud" ]
  set diving-patches-init-rock count patches with [ open-to-diving = true and bioc = "rock" ]
  set diving-patches-init-coral count patches with [ open-to-diving = true and bioc = "coral" ]
  set diving-patches-init-posi count patches with [ open-to-diving = true and bioc = "posi" ]
;diving attendance
  let nb-sites-init 50 ; number of diving sites at base year
  let divers/site/year-init 1300 ; number of divers by site by year at base year
  ; to assign the average number of divers by site by year at the patch level
  ask patches with [ open-to-diving = true ]
  [ set divers/diving-patch/year nb-sites-init / diving-patches-init * divers/site/year-init ]
  set tot-divers/diving-patches/year-init precision ( sum [ divers/diving-patch/year ] of patches with [ open-to-diving = true ] ) 0
;wind-turbines sites
  set wind-turbines-patches-init count patches with [ turbines = true ]

end

; to introduce the hypothesis of variation of phytoplankton biomass by the end of the century in %
to modify-B

  let horizon 2100 - 2018
  let phyto-obj phyto-init - ( ( - phyto-2100 ) * phyto-init / 100 )
  let phyto-horizon phyto-obj - phyto-init
  let phyto-incr phyto-horizon / horizon

  ifelse y <= 2018
  [ set phyto-year item 0 B
    set phyto-init phyto-year ]
  [ set phyto-year phyto-year + phyto-incr
    set B replace-item 0 B phyto-year ]

end

;to introduce the hypothesis of variation in fishing effort by type of fishery
to modify-C

  if y = 2019
  [ set B replace-item ( length B - 4 ) B ( item ( length B - 4 ) B * ( 1 + trawls-var / 100 ) )
    set trawls-new-demand item ( length B - 4 ) B
    set B replace-item ( length B - 3 ) B ( item ( length B - 3 ) B * ( 1 + lamparos-var / 100 ) )
    set lamparos-new-demand item ( length B - 3 ) B
    set B replace-item ( length B - 2 ) B ( item ( length B - 2 ) B * ( 1 + tuna-seiner-var / 100 ) )
    set tuna-seiner-new-demand item ( length B - 2 ) B
    set B replace-item ( length B - 1 ) B ( item ( length B - 1 ) B * ( 1 + other-artisanal-fisheries-var / 100 ) )
    set other-artisanal-fisheries-new-demand item ( length B - 1 ) B ]

end

; to reach the level of protection expected in each scenario,
; we downgrade every five years between 2020 and 2030
; the level of natural value required to be designated as an FPA -
; here these levels of natural value are chosen to get closer to the expected level of protection
; areas to be protected are designated after their natural value but rules of attribution slightly change among scenarios
to develop-FPA

let score 100
; systematic ban on fishing in the Cerbère-Banyuls Marine Nature Reserve
  if reserve = 56
  [ set no-go-take true
    set pcolor black ]

; when protecting a large portion of the MPA (S2),
; there is no need to target first a specific area:
; one is sure that all areas of great natural value will be included in the protected perimeter -
; here we rather seek to make progress on the overall MPA and the only criterion
; to be designated protected area refers to the level of natural value
  if zoning = "FPA -> 30%"
  [ if y = 2020 [ set score 16 ]
    if y = 2025 [ set score 13 ]
    if y = 2030 [ set score 10 ]

    if y = 2020 or y = 2025 or y = 2030
    [ if active or bioc = "pas de cor"
      [ if nat-rich-level > score
        [ set no-go-take true
          set pcolor black ]
      ]
    ]
; adjustment variable to achieve 30% of FPA by including some areas at the borders of existing FPAs
    if y = 2030
    [ if nat-rich-level > score
      [ ask neighbors with
        [ ( active or bioc = "pas de cor" ) and
          no-go-take != true and
          nat-rich-level > 9 ]
        [ set no-go-take true
          set pcolor black ]
      ]
    ]
  ]

; when protecting a small portion of the MPA (S2), one may want to make sure to protect
; consistent areas of great natural value rather than sparse micro hot points -
; to do so, we target the existing Marine Reserve and let new protected areas develop in its surroundings
  if zoning = "FPA -> 2%"
  [ if y = 2020
    [ if reserve = 56
      [ ask neighbors with
        [ ( active or bioc = "pas de cor" ) and
          no-go-take != true ]
        [ set no-go-take true
          set pcolor black ]
      ]
    ]
    if y = 2025 or y = 2030
    [ if pcolor = black
      [ ask patches with
        [ ( active or bioc = "pas de cor" ) and
          no-go-take != true
          and distance myself < 5 ]
        [ set no-go-take true
          set pcolor violet ]
      ]
    ]
    if y = 2026 or y = 2031
    [ if pcolor = violet
      [ set pcolor black ]
    ]
  ]

; when protecting a medium portion of the MPA (S3), we use a combination of the two previous rules:
; in 2020 we target the surroundings of the Marine Reserve to be sure to protect
; this area of greatest natural value while in 2025 and 2030 we also let protected areas develop elsewhere,
; after local level of natural value
 if zoning = "FPA -> 10%"
  [ if y = 2020
    [ if reserve = 56
      [ ask neighbors with
        [ ( active or bioc = "pas de cor" ) and
          no-go-take != true ]
        [ set no-go-take true
          set pcolor black ]
      ]
    ]
    if y = 2025 [ set score 20 ]
    if y = 2030 [ set score 15 ]
    if y = 2025 or y = 2030
    [ if pcolor = black
      [ ask patches with
        [ ( active or bioc = "pas de cor" ) and
          no-go-take != true
          and distance myself < 7 ]
        [ set no-go-take true
          set pcolor violet ]
      ]
      if active or bioc = "pas de cor"
      [ if nat-rich-level > score
        [ set no-go-take true
          set pcolor violet
          ask neighbors with
          [ ( active or bioc = "pas de cor" ) and
            no-go-take != true ]
          [ set no-go-take true
            set pcolor violet ]
        ]
      ]
    ]
    if y = 2026 or y = 2031
    [ if pcolor = violet
      [ set pcolor black ]
    ]
  ]

; prohibit fishing and diving and FPA
  if no-go-take = true and active
  [ let prohibit []
    set prohibit ( range 1 5 )
    ( foreach prohibit [ [ ? ] -> set B replace-item ( length B - ? ) B
      ( item ( length B - ? ) B * ( 1 - 100 / 100 ) ) ] )
    if open-to-diving = true
    [ set open-to-diving false ]
  ]

end

; to calculate species biomass using a combination of two types of controlling factors
to compute-B

  set B-matrix matrix:from-row-list n-values ( length B ) [ B ]
;to compute gross-diet from gross-predation-detail to take into account predatory pressure
  set gross-predation-detail multiply predation-ratio Q-2/B-matrix B-matrix
  set gross-diet map [ i -> sum i ] ( matrix:to-row-list gross-predation-detail )
;to compute gross-predation from gross-diet-detail to take into account growth potential
  set gross-diet-detail multiply diet-ratio Q/B-matrix B-matrix
  check-fishing-poss
  set gross-predation map [ i -> sum i ] ( matrix:to-row-list gross-diet-detail )
;to consider other preys/predators
;residual known preys/predators of group of species
;that are not represented in a given ecosystem
;are supposed eaten/fed anyway
  set P-1 P diet-ratio B Q/B
  set P-2 P predation-ratio B Q-2/B
;to compute generall biomass
;bottom-up control, positive feedback
  set B-prey []
  ( foreach gross-diet P-1 Q/B B-copy [ [ ?1 ?2 ?3 ?4 ]
    -> ifelse ?3 = 0
    [ set B-prey lput ?4 B-prey ]
    [ set B-prey lput ( ( ?1 + ?2 ) / ?3 ) B-prey ] ] )
;top-down control, negative feedback
  ifelse ticks = 0
  [
  set B-preda []
  ( foreach gross-predation P-2 Q-2/B B-copy [ [ ?1 ?2 ?3 ?4 ]
    -> ifelse ?3 = 0
      [ set B-preda lput ?4 B-preda ]
      [ set B-preda lput ( ( ?1 + ?2 ) / ?3 ) B-preda ] ] )
    set gross-predation-copy gross-predation
    set P-2-copy P-2
  ]
  [
  set B-preda []
  ( foreach gross-predation P-2 Q-2/B B-copy gross-predation-copy P-2-copy [ [ ?1 ?2 ?3 ?4 ?5 ?6 ]
    -> ifelse ?3 = 0
      [ set B-preda lput ?4 B-preda ]
      [ ifelse ( ?5 + ?6 ) - 1 <= 0
        [ set B-preda lput 0 B-preda ]
        [ set B-preda lput ( ?4 * ( 1 - ( ( ?1 + ?2 ) / ( ?5 + ?6 ) - 1 ) ) ) B-preda ] ] ] )
  ]
;combination - here equal but could be made different
;to give more weight to one or another controlling factor
  set B-end []
  ( foreach B-prey B-preda [ [ ?1 ?2 ] -> ifelse ( ( ?1 + ?2 ) / 2 ) <= 0
    [ set B-end lput 0 B-end ]
    [ set B-end lput ( ( ?1 + ?2 ) / 2 ) B-end ] ] )
;to compute special biomass
;phyto
  ifelse phyto-change?
  [ set B-end replace-item 0 B-end phyto-year ]
  [ set B-end replace-item 0 B-end item 0 B-copy ]
;detritus
  if ( sum B-copy - item 2 B-copy ) * ( sum B-end - item 2 B-end ) != 0
  [ set B-end replace-item 2 B-end ( item 2 B-copy / ( sum B-copy - item 2 B-copy ) * ( sum B-end - item 2 B-end ) ) ]
;fishing
  set B-end replace-item ( length B - 4 ) B-end sum matrix:get-column checked-fished-detail 0
  set B-end replace-item ( length B - 3 ) B-end sum matrix:get-column checked-fished-detail 1
  set B-end replace-item ( length B - 2 ) B-end sum matrix:get-column checked-fished-detail 2
  set B-end replace-item ( length B - 1 ) B-end sum matrix:get-column checked-fished-detail 3
;to compute total fishing
  set C-end []
  set C-end map [ i -> sum i ] ( matrix:to-row-list checked-fished-detail )
;to save each year results for next year computing
  set B B-end
  set C C-end
;to save base year results for comparing
  if y = 2018
  [ set B-2018 B-end
    set C-2018 C-end
    set fished-biomass-2018 checked-fished-detail ]

end

; if "old", reefs villages may be densified
to densi-reefs

  let horizon 2050 - 2018
  let dens-obj 50 ; density of a reef village by 2050 = 50%
  let dens-horizon dens-obj - dens-init
  let dens-incr dens-horizon / horizon
  let items range length B

  ifelse y <= 2018
  [ set dens-year dens-init ]
  [ set dens-year dens-year + dens-incr
    ( foreach items [ ? ->  set B replace-item ? B
      ( item ? reefs-dens-max-detail * dens-year / 100 ) ] ) ]

  set reefs-dens precision ( sum B / reefs-dens-max * 100 ) 0
; to overwrite concerned data
  compute-output-data

end

; if "old", reefs villages may be prohibited to fishing
to forbid-fishing-reefs

  let prohibit []
  set prohibit ( range 1 5 )
  if y > 2018
  [ ( foreach prohibit [ ? -> set B replace-item ( length B - ? ) B
      ( item ( length B - ? ) B * ( 1 - 100 / 100 ) ) ] ) ]

  set reefs-dens precision ( sum B / reefs-dens-max * 100 ) 0
; to overwrite concerned data
  compute-output-data

end

; reefs villages may be temporarily repopulated with heritage species (S1)
to repop-reefs-S1

  let grouper-B 56.385
  let grouper-diet-ratio [ 0 0.07 0 0 0.26 0.17 0 0.03 0 0.09 0.05 0 0.11 0 0 0 0 0.02 0 0 0 0 0 ]
  let grouper-Q/B 5.95

  if y = 2020
  [ set B replace-item ( length B - 5 ) B ( dens-init * grouper-B / 100 )
    matrix:set-column diet-ratio ( length B - 5 ) grouper-diet-ratio
    set Q/B replace-item ( length Q/B - 5 ) Q/B grouper-Q/B
; to format the new input data
  compute-input-data
    set B-copy B ]

  if y = 2021 or y = 2022 or y = 2023 or y = 2024
  [ set B replace-item ( length B - 5 ) B ( item ( length B - 5 ) B + ( dens-init * grouper-B / 100 ) ) ]

  set reefs-dens precision ( sum B / reefs-dens-max * 100 ) 0
; to overwrite concerned data
  compute-output-data

end

; reefs villages may be temporarily repopulated with commercial species (S1)
to repop-reefs-S2

  let dentex-B 58.032
  let dentex-diet-ratio [ 0 0.07 0 0 0.26 0.17 0 0.03 0 0.09 0.05 0 0.11 0 0 0 0 0.02 0 0 0 0 0 ]
  let dentex-Q/B 5.95
  let dentex-fished-detail [ 0.00152741 7.9004E-05 0 0.001474741 ]
  let seabass-B+ 44.42

  if y = 2020
  [
    set B replace-item ( length B - 8 ) B ( item ( length B - 8 ) B + ( dens-init * seabass-B+ / 100 ) )
    set B replace-item ( length B - 5 ) B ( dens-init * dentex-B / 100 )
    matrix:set-column diet-ratio ( length B - 5 ) dentex-diet-ratio
    set Q/B replace-item ( length Q/B - 5 ) Q/B dentex-Q/B
    matrix:set-row checked-fished-detail ( length B - 5 ) dentex-fished-detail
    let sum-fished-detail map [ i -> sum i ] ( matrix:to-column-list checked-fished-detail )
    let sum-fished-detail-matrix matrix:from-row-list n-values ( length B ) [ sum-fished-detail ]
    let new-fished-detail matrix:from-row-list ( map divide matrix:to-row-list checked-fished-detail matrix:to-row-list sum-fished-detail-matrix )
    set diet-ratio switch-columns ( range 4 ) ( range ( length B - 4 ) ( length B ) ) new-fished-detail diet-ratio
; to format the new input data
    compute-input-data
    set B-copy B
  ]

  if y = 2021 or y = 2022 or y = 2023 or y = 2024
  [
    set B replace-item ( length B - 8 ) B ( item ( length B - 8 ) B + ( dens-init * seabass-B+ / 100 ) )
    set B replace-item ( length B - 5 ) B ( item ( length B - 5 ) B + ( dens-init * dentex-B / 100 ) )
  ]

  set reefs-dens precision ( sum B / reefs-dens-max * 100 ) 0
; to overwrite concerned data
  compute-output-data

end

; reefs villages may be extended
to grow-reefs

  let dens-obj 50 ; density of a reef village by 2050 = 50%
  let items []
  set items range length B
  let reefs-step []

  if new-reefs-fishing = "limited"
  [
    if y = 2019 or y = 2020 or y = 2021 [ set reefs-step ( range 5 length B ) ]
    if y = 2022 or y = 2023 [ set reefs-step ( range 11 length B ) ]
    if y >= 2024 [ set reefs-step ( range 18 ( length B - 1 ) ) ] ; artisanal fisheries only

    if y >= 2019
    [
    foreach items [ ? -> set B replace-item ? B
      ( item ? reefs-dens-max-detail * dens-obj / 100 ) ]
    foreach reefs-step [ ? -> set B replace-item ? B 0 ]
    ]
  ]

  if new-reefs-fishing = "extended"
  [
    if y = 2019 or y = 2020 or y = 2021 [ set reefs-step ( range 5 length B ) ]
    if y = 2022 or y = 2023 [ set reefs-step ( range 11 length B ) ]
    if y >= 2024 [ set reefs-step ( range 18 ( length B - 5 ) ) ] ; all kinds of fishing

    if y >= 2019
    [
    foreach items [ ? -> set B replace-item ? B
      ( item ? reefs-dens-max-detail * dens-obj / 100 ) ]
    foreach reefs-step [ ? -> set B replace-item ? B 0 ]
    ]
  ]

  set reefs-dens precision ( sum B / reefs-dens-max * 100 ) 0
; to overwrite concerned data
  compute-output-data

end

; input data formatting when introducing new data durign the simulation
to compute-input-data

;to compute Q
  set Q []
  ( foreach B Q/B [ [ ?1 ?2 ] -> set Q lput ( ?1 * ?2 ) Q ] )
  set Q-matrix matrix:from-row-list n-values ( length B ) [ Q ]
  set Q/B-matrix matrix:from-row-list n-values ( length B ) [ Q/B ]
;to compute diet
  set diet-matrix matrix:times-element-wise diet-ratio Q-matrix
;to transpose diet -> to compute predation
  set diet-matrix-2 matrix:transpose diet-matrix
;to compute Q-2
  set Q-2 map [ i -> sum i ] ( matrix:to-column-list diet-matrix-2 )
;to compute Q-2/B
  set Q-2/B []
  ( foreach Q-2 B [ [ ?1 ?2 ] -> ifelse ?2 = 0
    [ set Q-2/B lput 0 Q-2/B ]
    [ set Q-2/B lput ( ?1 / ?2 ) Q-2/B ] ] )
  set Q-2/B-matrix matrix:from-row-list n-values ( length B ) [ Q-2/B ]
;to compute predation
  set Q-2-matrix matrix:from-row-list n-values ( length B ) [ Q-2 ]
  set predation-ratio matrix:from-row-list ( map divide matrix:to-row-list diet-matrix-2 matrix:to-row-list Q-2-matrix )

end

; output data formatting when introducing new data durign the simulation
to compute-output-data

;to recreate C and checked-fished-detail at each time step
  set B-matrix matrix:from-row-list n-values ( length B ) [ B ]
  set gross-diet-detail multiply diet-ratio Q/B-matrix B-matrix
  check-fishing-poss
  set C []
  set C map [ i -> sum i ] ( matrix:to-row-list checked-fished-detail )

;to recreate B-2018, C-2018, fished-biomass-2018
  if y = 2018
  [
    set B-2018 B
    set C-2018 C
    set fished-biomass-2018 checked-fished-detail
  ]

end

; to check there's enough species biomass to be fished
to check-fishing-poss

; gross fishing demand, including assumption of variation in fishing effort
  set fished-detail matrix:submatrix gross-diet-detail 0 ( length B - 4 ) ( length B ) ( length B )
  set predation-detail matrix:submatrix gross-diet-detail 0 0 ( length B ) ( length B - 4 )
  let B-C-matrix matrix:from-column-list n-values 4 [ B ]
  set checked-fished-detail matrix:from-row-list ( map update matrix:to-row-list B-C-matrix matrix:to-row-list fished-detail )
; available gross biomass for fishing - if target species abundance = 0 -> no fishing
  matrix:set-column gross-diet-detail ( length B - 4 ) matrix:get-column checked-fished-detail 0
  matrix:set-column gross-diet-detail ( length B - 3 ) matrix:get-column checked-fished-detail 1
  matrix:set-column gross-diet-detail ( length B - 2 ) matrix:get-column checked-fished-detail 2
  matrix:set-column gross-diet-detail ( length B - 1 ) matrix:get-column checked-fished-detail 3

end

; to assign a cultural function to new reefs villages (S3)
; and compute the number of associated divers and of new organizations
to dive-reefs

  let trips/day/site 2
  let divers/trip 16
  let site-entry-points 2
  let divers/day/site trips/day/site * divers/trip * site-entry-points
  let days/season 120
  let divers/organization/year 3000

  let opening-years [ 2025 2035 ] ; opening years for diving recreational reefs
  let reefs-pub [ 1 2 ] ; referring to specific reefs villages - see setup-GIS
  ( foreach opening-years reefs-pub [ [ ?1 ?2 ] -> if y = ?1
    [ if reefs-public = ?2
      [
        set divers/diving-patch/year divers/day/site * days/season
        set organization-number divers/diving-patch/year / divers/organization/year
  ] ] ] )

end

; to develop a commercial windfarm, assign it a cultural function
; and compute the number of associated sea-visitors
to sea-visit

  let nb-turbines/ferme-com 80
  let sea-visit/jour 2
  let passager/sea-visit 75
  let passager/jour sea-visit/jour * passager/sea-visit
  let jour/saison 60
  let max-passager/an passager/jour * jour/saison

  if y = 2020 or y = 2025 or y = 2030 or y = 2035 or y = 2040 or y = 2045
  [ if count patches with [ turbines = true ] < nb-turbines/ferme-com
    [ if turbines = true
      [ ifelse any? neighbors with [ turbines-area = "général" or turbines-area = "acceptable" and turbines != true ]
        [ ask one-of neighbors with [ turbines-area = "général" or turbines-area = "acceptable" and turbines != true ]
          [ set turbines true ] ]
        [ if any? neighbors with [ turbines != true ]
          [ ask one-of neighbors with [ turbines != true ]
            [ set turbines true ] ] ]
      ]
      set pcolor violet + 1
   ]
  ]

  set wind-turbines-patches count patches with [ turbines = true ]
  set tot-passenger/year ( wind-turbines-patches / nb-turbines/ferme-com * max-passager/an )

end

;;;;;;;;;;;;
;;;;PLOT;;;;
;;;;;;;;;;;;

to launch-plots

  let name []
  let items []

  set-current-plot "sand"
  set name [ "phytoplankton" "zooplankton" "detritus" "microphytobenthos" "worms" "suprabenthos" "bivalves-and-gastropods" "benthic-invertebrates" "lobsters" "crabs" "echinoderms" "small-pelagic-fish" "soles" "fish-feeding-on-polychaetes-bivalve-and-gastropods" "octopuses" "cuttlefish-and-squids" "gilthead-seabream" "european-seabass" "red-mullets" "fish-feeding-on-benthic-crustaceans" "horse-mackerel" "mackerel" "blue-whiting" "hake" "atlantic-bluefin-tuna" "other-large-pelagic-fishes" "anglerfish" "rays-and-skates" "sharks" "baleen-whales" "dolphins" "trawls" "lamparos" "tuna-seiner" "other-artisanal-fisheries" ]
  set items range length name
  ( foreach name items
    [ [ ?1 ?2 ] -> set-current-plot-pen ?1 plot sum [ item ?2 B ] of patches with [ bioc = "sand" ] ] )

  set-current-plot "mud"
  set name [ "phytoplankton" "zooplankton" "detritus" "microphytobenthos" "worms" "suprabenthos" "bivalves-and-gastropods" "benthic-invertebrates" "lobsters" "crabs" "echinoderms" "small-pelagic-fish" "soles" "fish-feeding-on-polychaetes-bivalve-and-gastropods" "octopuses" "cuttlefish-and-squids" "gilthead-seabream" "european-seabass" "red-mullets" "fish-feeding-on-benthic-crustaceans" "horse-mackerel" "mackerel" "blue-whiting" "hake" "atlantic-bluefin-tuna" "other-large-pelagic-fishes" "anglerfish" "rays-and-skates" "sharks" "baleen-whales" "dolphins" "trawls" "lamparos" "tuna-seiner" "other-artisanal-fisheries" ]
  set items range length name
  ( foreach name items
    [ [ ?1 ?2 ] -> set-current-plot-pen ?1 plot sum [ item ?2 B ] of patches with [ bioc = "mud" ] ] )

  set-current-plot "rock"
  set name [ "phytoplankton" "zooplankton" "detritus" "benthic-macrophytes" "worms" "suprabenthos" "white-gorgonian" "benthic-invertebrates" "echinoderms" "octopuses" "bivalves-and-gastropods" "salema" "fish-feeding-on-polychaetes-bivalve-and-gastropods" "gilthead-seabream" "european-conger" "european-seabass" "scorpion-fish" "picarel" "grouper/dentex" "trawls" "lamparos" "tuna-seiner" "other-artisanal-fisheries" ]
  set items range length name
  ( foreach name items
    [ [ ?1 ?2 ] -> set-current-plot-pen ?1 plot sum [ item ?2 B ] of patches with [ bioc = "rock" ] ] )

  set-current-plot "posi"
  set name [ "phytoplankton" "zooplankton" "detritus" "posidonia" "bivalves-and-gastropods" "suprabenthos" "salema" "echinoderms" "crabs" "fish-feeding-on-polychaetes-bivalve-and-gastropods" "gilthead-seabream" "octopuses" "cuttlefish-and-squids" "european-conger" "scorpion-fish" "european-seabass" "picarel" "trawls" "lamparos" "tuna-seiner" "other-artisanal-fisheries" ]
  set items range length name
  ( foreach name items
    [ [ ?1 ?2 ] -> set-current-plot-pen ?1 plot sum [ item ?2 B ] of patches with [ bioc = "posi" ] ] )

  set-current-plot "coral"
  set name [ "phytoplankton" "zooplankton" "detritus" "benthic-macrophytes" "worms" "benthic-invertebrates" "echinoderms" "bivalves-and-gastropods" "planctonophagous-fish" "fish-feeding-on-polychaetes-bivalve-and-gastropods" "octopuses" "european-seabass" "scorpion-fish" "suprabenthos" "lobsters" "trawls" "lamparos" "tuna-seiner" "other-artisanal-fisheries" ]
  set items range length name
  ( foreach name items
    [ [ ?1 ?2 ] -> set-current-plot-pen ?1 plot sum [ item ?2 B ] of patches with [ bioc = "coral" ] ] )

  set-current-plot "undersea & fished biomass"

  set-current-plot-pen "undersea" plot sum
  [ sum B - ( item ( length B - 1 ) B + item ( length B - 2 ) B + item ( length B - 3 ) B + item ( length B - 4 ) B ) ]
  of patches with [ active = true ]

  set-current-plot-pen "trawls" plot sum [ item ( length B - 4 ) B ]
  of patches with [ active = true ]
  set-current-plot-pen "lamparos" plot sum [ item ( length B - 3 ) B ]
  of patches with [ active = true ]
  set-current-plot-pen "tuna-seiner" plot sum [ item ( length B - 2 ) B ]
  of patches with [ active = true ]
  set-current-plot-pen "other-artisanal-fisheries" plot sum [ item ( length B - 1 ) B ]
  of patches with [ active = true ]

  set-current-plot "densi-reefs"
  set name [ "phytoplankton" "zooplankton" "detritus" "benthic-macrophytes" "worms" "suprabenthos" "white-gorgonian" "benthic-invertebrates" "echinoderms" "octopuses" "bivalves-and-gastropods" "salema" "fish-feeding-on-polychaetes-bivalve-and-gastropods" "gilthead-seabream" "european-conger" "european-seabass" "scorpion-fish" "picarel" "grouper/dentex" "trawls" "lamparos" "tuna-seiner" "other-artisanal-fisheries" ]
  set items range length name
  ( foreach name items
    [ [ ?1 ?2 ] -> set-current-plot-pen ?1 plot sum [ item ?2 B ] of patches with [ reefs-state = "old" ] ] )

  set-current-plot "repop-reefs"
  set name [ "phytoplankton" "zooplankton" "detritus" "benthic-macrophytes" "worms" "suprabenthos" "white-gorgonian" "benthic-invertebrates" "echinoderms" "octopuses" "bivalves-and-gastropods" "salema" "fish-feeding-on-polychaetes-bivalve-and-gastropods" "gilthead-seabream" "european-conger" "european-seabass" "scorpion-fish" "picarel" "grouper/dentex" "trawls" "lamparos" "tuna-seiner" "other-artisanal-fisheries" ]
  set items range length name
  ( foreach name items
    [ [ ?1 ?2 ] -> set-current-plot-pen ?1 plot sum [ item ?2 B ] of patches with [ reefs-repop = true ] ] )

  set-current-plot "grow-reefs"
  set name [ "phytoplankton" "zooplankton" "detritus" "benthic-macrophytes" "worms" "suprabenthos" "white-gorgonian" "benthic-invertebrates" "echinoderms" "octopuses" "bivalves-and-gastropods" "salema" "fish-feeding-on-polychaetes-bivalve-and-gastropods" "gilthead-seabream" "european-conger" "european-seabass" "scorpion-fish" "picarel" "grouper/dentex" "trawls" "lamparos" "tuna-seiner" "other-artisanal-fisheries" ]
  set items range length name
  if any? patches with [ reefs-state = "new" ]
  [ ( foreach name items
    [ [ ?1 ?2 ] -> set-current-plot-pen ?1 plot sum [ item ?2 B ] of patches with [ reefs-state = "new" ] ] ) ]

end

;;;;;;;;;;;
;;;;GIS;;;;
;;;;;;;;;;;

to setup-GIS

  set environment gis:load-dataset "environment.shp"
  set habitats gis:load-dataset "habitats.shp"
  set bathy gis:load-dataset "bathy.asc"
  set milles gis:load-dataset "milles.asc"
  set coastline gis:load-dataset "coastline.shp"
  set lakes-lagoons gis:load-dataset "lakes-lagoons.shp"
  set white-gorgonian gis:load-dataset "white-gorgonian.shp"
  set reefs-villages gis:load-dataset "reefs-villages.shp"
  set buffer gis:load-dataset "buffer.shp"
  set nat-rich gis:load-dataset "nat-rich.shp"
  set marine-reserve gis:load-dataset "marine-reserve.shp"
  set windfarm gis:load-dataset "windfarm.shp"
  set wind-anchors gis:load-dataset "wind-anchors.shp"
  set wind-turbines gis:load-dataset "wind-turbines.shp"
  set diving gis:load-dataset "diving.shp"
  gis:set-world-envelope ( gis:envelope-union-of
    ( gis:envelope-of environment )
    ( gis:envelope-of habitats )
    ( gis:envelope-of bathy )
    ( gis:envelope-of milles )
    ( gis:envelope-of coastline )
    ( gis:envelope-of lakes-lagoons )
    ( gis:envelope-of white-gorgonian )
    ( gis:envelope-of reefs-villages )
    ( gis:envelope-of buffer )
    ( gis:envelope-of nat-rich )
    ( gis:envelope-of marine-reserve )
    ( gis:envelope-of windfarm )
    ( gis:envelope-of wind-anchors )
    ( gis:envelope-of wind-turbines )
    ( gis:envelope-of diving ) )

  gis:apply-coverage habitats "LIB_EBQI" bioc
  gis:apply-coverage marine-reserve "OBJECTID" reserve
  gis:apply-coverage nat-rich "TABLE_SENS" nat-rich-level
  gis:apply-coverage wind-turbines "ID_EOLIENN" turbines
  ask patches gis:intersecting gis:find-features wind-turbines
  "ID_EOLIENN" "*" [ set turbines true ]
  gis:apply-coverage windfarm "ZONE_PROP" turbines-area
  gis:apply-coverage diving "NOM" open-to-diving
  ask patches gis:intersecting gis:find-features diving
  "NOM" "*" [ set open-to-diving true ]

  gis:apply-raster bathy bat
  gis:apply-raster milles mil

  ask patches [ set active false ]

  ask patches gis:intersecting gis:find-features environment
  "id" "1" [ set pcolor blue ]

  ask patches gis:intersecting gis:find-features lakes-lagoons
  "Object_ID" "6" [ set pcolor blue ]
  ask patches gis:intersecting gis:find-features lakes-lagoons
  "Object_ID" "7" [ set pcolor blue ]
  ask patches gis:intersecting gis:find-features lakes-lagoons
  "Object_ID" "10" [ set pcolor blue ]
  ask patches gis:intersecting gis:find-features lakes-lagoons
  "Object_ID" "14" [ set pcolor blue ]
  ask patches gis:intersecting gis:find-features lakes-lagoons
  "Object_ID" "16" [ set pcolor blue ]

  ask patches gis:intersecting gis:find-features coastline
  "REGION" "LANGUEDOC-ROUSSILLON" [ set pcolor yellow - 3 ]
  ask patches gis:intersecting gis:find-features coastline
  "REGION" "Catalogne (nord)" [ set pcolor yellow - 3 ]

  ask patches gis:intersecting gis:find-features habitats
  "lib_EBQI" "pas de cor" [
    set bioc "pas de cor"
    set pcolor white ]
  ask patches gis:intersecting gis:find-features habitats
  "lib_EBQI" "vase" [
    set bioc "mud"
    set active true
    set pcolor brown ]
  ask patches gis:intersecting gis:find-features habitats
  "lib_EBQI" "sable" [
    set bioc "sand"
    set active true
    set pcolor yellow ]
  ask patches gis:intersecting gis:find-features habitats
  "lib_EBQI" "rock" [
    set bioc "rock"
    set active true
    set pcolor grey ]
  ask patches gis:intersecting gis:find-features habitats
  "lib_EBQI" "phanerogam" [
    set bioc "posi"
    set active true
    set pcolor green ]
  ask patches gis:intersecting gis:find-features habitats
  "lib_EBQI" "coral" [
    set bioc "coral"
    set active true
    set pcolor red ]

  gis:apply-coverage white-gorgonian "Z" gorg
  ask patches with [ gorg > 0 ] [
    set bioc "rock"
    set active true
  ]

  gis:apply-coverage reefs-villages "AREA_NAME" reefs
  ask patches gis:intersecting gis:find-features reefs-villages
  "AREA_NAME" "*" [
    set reefs true
    set bioc "rock"
    set pcolor grey - 2
    set reefs-cult false
    set reefs-repop false
  ]
  ask patches gis:intersecting gis:find-features reefs-villages
  "AREA_NAME" "Arg" [
    set reefs-state "new"
  ]
  ask patches gis:intersecting gis:find-features reefs-villages
  "AREA_NAME" "Leu3" [
    set reefs-state "old"
    set reefs-cult true
    set reefs-public 1
  ]
  ask patches gis:intersecting gis:find-features reefs-villages
  "AREA_NAME" "Can" [
    set reefs-state "old"
    set reefs-cult true
    set reefs-public 2
  ]
  ask patches gis:intersecting gis:find-features reefs-villages
  "AREA_NAME" "StCyp" [
    set reefs-state "old"
    set reefs-cult true
    set reefs-public 3
  ]
  ask patches gis:intersecting gis:find-features reefs-villages
  "AREA_NAME" "Bar2" [
    set reefs-repop true
  ]
  ask patches gis:intersecting gis:find-features reefs-villages
  "AREA_NAME" "Leu2" [
    set reefs-repop true
  ]

  display-windfarm
  display-wind-turbines

end

to display-habitats

  gis:set-drawing-color white
  gis:draw habitats 1

end

to display-trame

  ask patches [
    sprout 1 [
      set shape "contour"
      set color grey
      __set-line-thickness 0.01
    ]
  ]

end

to display-bathy

  gis:paint bathy 0
  let min-bat gis:minimum-of bathy
  let max-bat gis:maximum-of bathy
  ask patches
  [ if ( bat <= 0 ) or ( bat >= 0 )
    [ set pcolor scale-color black bat min-bat max-bat ] ]

end

to display-milles

  let min-milles gis:minimum-of milles
  let max-milles gis:maximum-of milles
  ask patches
  [ if ( mil <= 0 ) or ( mil >= 0 )
    [ set pcolor scale-color black mil max-milles min-milles ] ]
  gis:set-drawing-color blue
  gis:draw buffer 2

end

to display-white-gorgonian

  ask patches with [ gorg > 0 ] [ set pcolor white ]

end

to display-reefs

  gis:set-drawing-color black
  gis:draw reefs-villages 1

end

to display-windfarm

  gis:set-drawing-color black
  gis:draw windfarm 1
  ask patches with [ turbines-area = "général" ] [ set pcolor gray - 1 ]
  ask patches with [ turbines-area = "acceptable" ] [ set pcolor gray + 1 ]

end

to display-wind-anchors

  gis:set-drawing-color violet - 1
  gis:draw wind-anchors 1

end

to display-wind-turbines

  gis:set-drawing-color violet + 1
  gis:draw wind-turbines 1

end

to display-nat-rich

  ask patches with [ nat-rich-level >= 0 ] [ set pcolor scale-color red nat-rich-level 25 0 ]

end

to display-amp

;  gis:set-drawing-color black
;  gis:draw marine-reserve 1
  ask patches with [ reserve = 56 ] [ set pcolor orange ]

end

to display-open-to-divingeurs

  gis:set-drawing-color white
  gis:draw diving 1

end

;;;;;;;;;;;;;;
;;;;OUTPUT;;;;
;;;;;;;;;;;;;;

to print-results

  show-ecosystem-share
  show-FPA-share
  compare-phyto
  compare-biomass
  compare-fishing
  compare-undersea-biomass
  compare-biomass-detail
  compare-fishing-detail
  compare-diving

end

; share of each habitat in the overall MPA's ecosystem
to show-ecosystem-share

  print "number of patches describing the overall MPA's ecosystem"
  print count patches with [ active = true ]

  print "number of patches describing the sand ecosystem"
  print count patches with [ bioc = "sand" ]
  print "share of the sand ecosystem"
  ecosystem-share "sand"

  print "number of patches describing the mud ecosystem"
  print count patches with [ bioc = "mud" ]
  print "share of the mud ecosystem"
  ecosystem-share "mud"

  print "number of patches describing the rock ecosystem"
  print count patches with [ bioc = "rock" ]
  print "share of the rock ecosystem"
  ecosystem-share "rock"

  print "number of patches describing the posidonia ecosystem"
  print count patches with [ bioc = "posi" ]
  print "share of the posidonia ecosystem"
  ecosystem-share "posi"

  print "number of patches describing the coralligenous ecosystem"
  print count patches with [ bioc = "coral" ]
  print "share of the coralligenous ecosystem"
  ecosystem-share "coral"

end

to ecosystem-share [ designated-habitat ]

  print precision ( count patches with [ bioc = designated-habitat ] / count patches with [ active = true ] * 100 ) 2

end

; share of each habitat under strong protection
to show-FPA-share

  print "share of the MPA's overall ecosystem classified as a FPA"
  print precision ( FPA-% ) 2

  print "share of the sand ecosystem classified as a FPA"
  FPA-share "sand"

  print "share of the mud ecosystem classified as a FPA"
  FPA-share "mud"

  print "share of the rock ecosystem classified as a FPA"
  FPA-share "rock"

  print "share of the posidonia ecosystem classified as a FPA"
  FPA-share "posi"

  print "share of the coralligenous ecosystem classified as a FPA"
  FPA-share "coral"

end

to FPA-share [ designated-habitat ]

  print precision ( count patches with [ bioc = designated-habitat and no-go-take = true ]
    / count patches with [ bioc = designated-habitat ] * 100 ) 2

end

; overall variation of phytoplankton biomass density
to compare-phyto

  print "overall variation of phytoplankton biomass density"
  print precision ( (
    ( sum [ item 0 B ] of patches with
    [ active = true ] )
    /
    ( sum [ item 0 B-2018 ] of patches with
    [ active = true ] )
    - 1 ) * 100 ) 2

end

; total biomass variation of the overall MPA's ecosystem over
; the simulation period (here 2018 - 2050) / possibly by habitat
to compare-biomass

  print "total biomass variation of the overall MPA's ecosystem (undersea & fished)"
  print precision ( (
    ( sum [ sum B ] of patches with
    [ active = true ] )
    /
    ( sum [ sum B-2018 ] of patches with
    [ active = true ] )
    - 1 ) * 100 ) 2

  print "total biomass variation of the sand ecosystem (undersea & fished)"
  compare-B "sand"

  print "total biomass variation of the mud ecosystem (undersea & fished)"
  compare-B "mud"

  print "total biomass variation of the rock ecosystem (undersea & fished)"
  compare-B "rock"

  print "total biomass variation of the posidonia ecosystem (undersea & fished)"
  compare-B "posi"

  print "total biomass variation of the coralligenous ecosystem (undersea & fished)"
  compare-B "coral"

end

to compare-B [ designated-habitat ]

  print precision ( (
    ( sum [ sum B ] of patches with
    [ bioc = designated-habitat ] )
    /
    ( sum [ sum B-2018 ] of patches with
    [ bioc = designated-habitat ] )
    - 1 ) * 100 ) 2

end

; total variation of fished biomass for the overall MPA's ecosystem over
; the simulation period (here 2018 - 2050) / possibly by habitat
to compare-fishing

  print "total variation of fished biomass for the overall MPA's ecosystem"
  print precision ( (
    ( sum [ sum C ] of patches with
    [ active = true ] )
    /
    ( sum [ sum C-2018 ] of patches with
    [ active = true ] )
    - 1 ) * 100 ) 2

  print "total variation of fished biomass for the sand ecosystem"
  compare-C "sand"

  print "total variation of fished biomass for the mud ecosystem"
  compare-C "mud"

  print "total variation of fished biomass for the rock ecosystem"
  compare-C "rock"

  print "total variation of fished biomass for the posidonia ecosystem"
  compare-C "posi"

  print "total variation of fished biomass for the coralligenous ecosystem"
  compare-C "coral"

end

to compare-C [ designated-habitat ]

  print precision ( (
    ( sum [ sum C ] of patches with
      [ bioc = designated-habitat ] )
    /
    ( sum [ sum C-2018 ] of patches with
      [ bioc = designated-habitat ] )
    - 1 ) * 100 ) 2

end

; total variation of undersea biomass for the overall MPA's ecosystem over
; the simulation period (here 2018 - 2050) / possibly by habitat
to compare-undersea-biomass

  print "total variation of undersea biomass for the overall MPA's ecosystem"
  print precision ( (
    ( sum [ sum B - sum C ] of patches with
    [ active = true ] )
    /
    ( sum [ sum B-2018 - sum C-2018 ] of patches with
    [ active = true ] )
    - 1 ) * 100 ) 2

  print "total variation of undersea biomass for the sand ecosystem"
  compare-B-C "sand"

  print "total variation of undersea biomass for the mud ecosystem"
  compare-B-C "mud"

  print "total variation of undersea biomass for the rock ecosystem"
  compare-B-C "rock"

  print "total variation of undersea biomass for the posidonia ecosystem"
  compare-B-C "posi"

  print "total variation of undersea biomass for the coralligenous ecosystem"
  compare-B-C "coral"

end

to compare-B-C [ designated-habitat ]

  print precision ( (
    ( sum [ sum B - sum C ] of patches with
    [ bioc = designated-habitat ] )
    /
    ( sum [ sum B-2018 - sum C-2018 ] of patches with
    [ bioc = designated-habitat ] )
    - 1 ) * 100 ) 2
end

; variation of each species undersea biomass and aggregate catches by fishing gear
; by habitat over the simulation period (here 2018 - 2050)
to compare-biomass-detail

  print "variation of each species undersea biomass and aggregate catches by fishing gear for the sand ecosystem"
  let name []
  set name [ "0:phytoplankton" "1:zooplankton" "2:detritus" "3:microphytobenthos" "4:worms" "5:suprabenthos" "6:bivalves-and-gastropods" "7:benthic-invertebrates" "8:lobsters" "9:crabs" "10:echinoderms" "11:small-pelagic-fish" "12:soles" "13:fish-feeding-on-polychaetes-bivalve-and-gastropods" "14:octopuses" "15:cuttlefish-and-squids" "16:gilthead-seabream" "17:european-seabass" "18:red-mullets" "19:fish-feeding-on-benthic-crustaceans" "20:horse-mackerel" "21:mackerel" "22:blue-whiting" "23:hake" "24:atlantic-bluefin-tuna" "25:other-large-pelagic-fishes" "26:anglerfish" "27:rays-and-skates" "28:sharks" "29:baleen-whales" "30:dolphins" "31:trawls" "32:lamparos" "33:tuna-seiner" "34:other-artisanal-fisheries" ]
  comp-B-detail name "sand"

  print "variation of each species undersea biomass and aggregate catches by fishing gear for the mud ecosystem"
  set name [ "0:phytoplankton" "1:zooplankton" "2:detritus" "3:microphytobenthos" "4:worms" "5:suprabenthos" "6:bivalves-and-gastropods" "7:benthic-invertebrates" "8:lobsters" "9:crabs" "10:echinoderms" "11:small-pelagic-fish" "12:soles" "13:fish-feeding-on-polychaetes-bivalve-and-gastropods" "14:octopuses" "15:cuttlefish-and-squids" "16:gilthead-seabream" "17:european-seabass" "18:red-mullets" "19:fish-feeding-on-benthic-crustaceans" "20:horse-mackerel" "21:mackerel" "22:blue-whiting" "23:hake" "24:atlantic-bluefin-tuna" "25:other-large-pelagic-fishes" "26:anglerfish" "27:rays-and-skates" "28:sharks" "29:baleen-whales" "30:dolphins" "31:trawls" "32:lamparos" "33:tuna-seiner" "34:other-artisanal-fisheries" ]
  comp-B-detail name "mud"

  print "variation of each species undersea biomass and aggregate catches by fishing gear for the rock ecosystem"
  set name [ "0:phytoplankton" "1:zooplankton" "2:detritus" "3:benthic-macrophytes" "4:worms" "5:suprabenthos" "6:white-gorgonian" "7:benthic-invertebrates" "8:echinoderms" "9:octopuses" "10:bivalves-and-gastropods" "11:salema" "12:fish-feeding-on-polychaetes-bivalve-and-gastropods" "13:gilthead-seabream" "14:european-conger" "15:european-seabass" "16:scorpion-fish" "17:picarel" "18:grouper/dentex" "19:trawls" "20:lamparos" "21:tuna-seiner" "22:other-artisanal-fisheries" ]
  comp-B-detail name "rock"

  print "variation of each species undersea biomass and aggregate catches by fishing gear for the posidonia ecosystem"
  set name [ "0:phytoplankton" "1:zooplankton" "2:detritus" "3:posidonia" "4:bivalves-and-gastropods" "5:suprabenthos" "6:salema" "7:echinoderms" "8:crabs" "9:fish-feeding-on-polychaetes-bivalve-and-gastropods" "10:gilthead-seabream" "11:octopuses" "12:cuttlefish-and-squids" "13:european-conger" "14:scorpion-fish" "15:european-seabass" "16:picarel" "17:trawls" "18:lamparos" "19:tuna-seiner" "20:other-artisanal-fisheries" ]
  comp-B-detail name "posi"

  print "variation of each species undersea biomass and aggregate catches by fishing gear for the coralligenous ecosystem"
  set name [ "0:phytoplankton" "1:zooplankton" "2:detritus" "3:benthic-macrophytes" "4:worms" "5:benthic-invertebrates" "6:echinoderms" "7:bivalves-and-gastropods" "8:planctonophagous-fish" "9:fish-feeding-on-polychaetes-bivalve-and-gastropods" "10:octopuses" "11:european-seabass" "12:scorpion-fish" "13:suprabenthos" "14:lobsters" "15:trawls" "16:lamparos" "17:tuna-seiner" "18:other-artisanal-fisheries" ]
  comp-B-detail name "coral"

end

to comp-B-detail [ names designated-habitat ]

  let items []
  set items range length names

  let B-ecosys matrix:from-row-list [ B ] of patches with [ bioc = designated-habitat ]
  set B-ecosys map [ i -> sum i ] ( matrix:to-column-list B-ecosys )
  let B-ecosys-2018 matrix:from-row-list [ B-2018 ] of patches with [ bioc = designated-habitat ]
  set B-ecosys-2018 map [ i -> sum i ] ( matrix:to-column-list B-ecosys-2018 )

  let B-detail []
  set B-detail compare B-ecosys B-ecosys-2018

  let printer []
  set printer ( list items B-detail )

  print names
  print matrix:pretty-print-text matrix:from-row-list printer

end

; variation of each species catches by fishing gear
; by habitat over the simulation period (here 2018 - 2050)
to compare-fishing-detail

  print "variation of each species catches by fishing gear for the sand ecosystem"
  let name1 []
  set name1 ["0:trawls" "1:lamparos" "2:tuna-seiner" "3:other-artisanal-fisheries"]
  let name2 []
  set name2 [ "0:phyto" "1:zoo" "2:detritus" "3:microphytobenthos" "4:vers" "5:supra" "6:biv-gas" "7:invert" "8:lobsters" "9:crabes" "10:oursins" "\n11:petits-pélagiques" "12:sole" "13:autres-biv-gas-feeders" "14:poulpe" "15:couteaux-seiches" "16:dorade" "17:loup" "18:rougets-barbets" "19:autres-crusta-feeders" "\n20:chinchards" "21:maquereaux" "22:merlan" "23:merlu" "24:thon" "25:autres-grands-pélagiques" "\n26:bonites" "27:raies" "28:requins" "29:baleine" "30:dauphins" "31:trawls" "32:lamparos" "33:tuna-seiner" "34:other-artisanal-fisheries" ]
  compare-C-detail name1 name2 "sand"

  print "variation of each species catches by fishing gear for the mud ecosystem"
  set name1 ["0:trawls" "1:lamparos" "2:tuna-seiner" "3:other-artisanal-fisheries"]
  set name2 [ "0:phyto" "1:zoo" "2:detritus" "3:microphytobenthos" "4:vers" "5:supra" "6:biv-gas" "7:invert" "8:lobsters" "9:crabes" "10:oursins" "\n11:petits-pélagiques" "12:sole" "13:autres-biv-gas-feeders" "14:poulpe" "15:couteaux-seiches" "16:dorade" "17:loup" "18:rougets-barbets" "19:autres-crusta-feeders" "\n20:chinchards" "21:maquereaux" "22:merlan" "23:merlu" "24:thon" "25:autres-grands-pélagiques" "\n26:bonites" "27:raies" "28:requins" "29:baleine" "30:dauphins" "31:trawls" "32:lamparos" "33:tuna-seiner" "34:other-artisanal-fisheries" ]
  compare-C-detail name1 name2 "mud"

  print "variation of each species catches by fishing gear for the rock ecosystem"
  set name1 ["0:trawls" "1:lamparos" "2:tuna-seiner" "3:other-artisanal-fisheries"]
  set name2 [ "0:phyto" "1:zoo" "2:detritus" "3:MPOs" "4:vers" "5:supra" "6:gorgone" "7:invert" "8:oursins" "9:poulpe" "10:biv-gas" "11:saupe" "\n12:sparides" "13:dorade" "14:congre" "15:loup" "16:rascasse" "17:mendole" "18:merou/denti" "19:trawls" "20:lamparos" "21:tuna-seiner" "22:other-artisanal-fisheries" ]
  compare-C-detail name1 name2 "rock"

  print "variation of each species catches by fishing gear for the posidonia ecosystem"
  set name1 ["0:trawls" "1:lamparos" "2:tuna-seiner" "3:other-artisanal-fisheries"]
  set name2 [ "0:phyto" "1:zoo" "2:detritus" "3:posi" "4:biv-gas" "5:supra" "6:saupe" "7:oursins" "8:crabes" "\n9:sparides" "10:dorade" "11:poulpe" "12:couteaux-seiches" "13:congre" "14:rascasse" "15:loup" "16:mendole" "17:trawls" "18:lamparos" "19:tuna-seiner" "20:other-artisanal-fisheries" ]
  compare-C-detail name1 name2 "posi"

  print "variation of each species catches by fishing gear for the coralligenous ecosystem"
  set name1 ["0:trawls" "1:lamparos" "2:tuna-seiner" "3:other-artisanal-fisheries"]
  set name2 [ "0:phyto" "1:zoo" "2:detritus" "3:MPOs" "4:vers" "5:invert" "6:oursins" "7:biv-gas" "8:planctonophages" "\n9:sparides" "10:poulpe" "11:loup" "12:rascasse" "13:supra" "14:lobsters" "15:trawls" "16:lamparos" "17:tuna-seiner" "18:other-artisanal-fisheries" ]
  compare-C-detail name1 name2 "coral"

end

to compare-C-detail [ names1 names2 designated-habitat ]

  let items1 []
  set items1 range length names1
  let items2 []
  set items2 range length names2
  set items2 fput 0 items2

  let C-ecosys matrix:copy [ checked-fished-detail ] of one-of patches with [ bioc = designated-habitat ]
  set C-ecosys matrix:times C-ecosys 0
  foreach [ checked-fished-detail ] of patches with [ bioc = designated-habitat ]
  [ ? -> set C-ecosys matrix:plus ? C-ecosys ]

  let C-ecosys-2018 matrix:copy [ fished-biomass-2018 ] of one-of patches with [ bioc = designated-habitat ]
  set C-ecosys-2018 matrix:times C-ecosys-2018 0
  foreach [ fished-biomass-2018 ] of patches with [ bioc = designated-habitat ]
  [ ? -> set C-ecosys-2018 matrix:plus ? C-ecosys-2018 ]

  let C-detail []
  set C-detail (map compare matrix:to-row-list C-ecosys matrix:to-row-list C-ecosys-2018)
  set C-detail fput items1 C-detail
  set C-detail matrix:to-column-list matrix:from-row-list C-detail
  set C-detail fput items2 C-detail
  set C-detail matrix:from-column-list C-detail

  print "the first 0 in the upper left corner does not count \ncolumn -> fishing gears"
  print names1
  print "row -> fished species"
  print names2
  print matrix:pretty-print-text C-detail

end

; patch-only -> ask patch x y [ compare-B-patch ]
; variation of undersea and fished biomass for each species
; in a given patch over the simulation period (here 2018 - 2050)
to compare-B-patch

  let name ""
  if bioc = "sand" or bioc = "mud"
  [ set name "0:phyto 1:zoo 2:detritus 3:microphytobenthos 4:vers 5:supra 6:biv-gas 7:invert 8:lobsters 9:crabes 10:oursins \n11:petits-pélagiques 12:sole 13:autres-biv-gas-feeders 14:poulpe 15:couteaux-seiches 16:dorade 17:loup 18:rougets-barbets 19:autres-crusta-feeders \n20:chinchards 21:maquereaux 22:merlan 23:merlu 24:thon 25:autres-grands-pélagiques \n26:bonites 27:raies 28:requins 29:baleine 30:dauphins 31:trawls 32:lamparos 33:tuna-seiner 34:other-artisanal-fisheries" ]
  if bioc = "rock"
  [ set name "0:phyto 1:zoo 2:detritus 3:MPOs 4:vers 5:supra 6:gorgone 7:invert 8:oursins 9:poulpe 10:biv-gas 11:saupe \n12:sparides 13:dorade 14:congre 15:loup 16:rascasse 17:mendole 18:merou/denti 19:trawls 20:lamparos 21:tuna-seiner 22:other-artisanal-fisheries" ]
  if bioc = "posi"
  [ set name "0:phyto 1:zoo 2:detritus 3:posi 4:biv-gas 5:supra 6:saupe 7:oursins 8:crabes \n9:sparides 10:dorade 11:poulpe 12:couteaux-seiches 13:congre 14:rascasse 15:loup 16:mendole 17:trawls 18:lamparos 19:tuna-seiner 20:other-artisanal-fisheries" ]
  if bioc = "coral"
  [ set name "0:phyto 1:zoo 2:detritus 3:MPOs 4:vers 5:invert 6:oursins 7:biv-gas 8:planctonophages \n9:sparides 10:poulpe 11:loup 12:rascasse 13:supra 14:lobsters 15:trawls 16:lamparos 17:tuna-seiner 18:other-artisanal-fisheries" ]

  let B-detail []
  set B-detail compare B B-2018
  set B-detail matrix:from-row-list list range (length B) B-detail

  print name
  print matrix:pretty-print-text B-detail

end

; patch-only -> ask patch x y [ compare-C-patch ]
; variation of each species catches by fishing gear
; in a given patch over the simulation period (here 2018 - 2050)
to compare-C-patch

  let name ""
  set name "0:trawls 1:lamparos 2:tuna-seiner 3:other-artisanal-fisheries"
  let C-detail []
  set C-detail matrix:from-row-list (map compare matrix:to-row-list checked-fished-detail matrix:to-row-list fished-biomass-2018)

  print name
  print matrix:pretty-print-text C-detail

end

to compare-diving

  print "variation of the total number of divers compared to 2018"
  set tot-divers/diving-patches/year precision ( sum [ divers/diving-patch/year ] of patches with [ open-to-diving = true ] ) 0
  print precision ( ( tot-divers/diving-patches/year / tot-divers/diving-patches/year-init - 1 ) * 100 ) 0

  print "variation of the total number of patches accessible for diving"
  print precision ( ( count patches with [ open-to-diving = true ] /
    ( diving-patches-init-sand + diving-patches-init-mud + diving-patches-init-rock +
      diving-patches-init-posi + diving-patches-init-coral ) - 1 ) * 100 ) 0

  print "variation of the total number of patches accessible for diving in the sand ecosystem"
  ifelse diving-patches-init-sand != 0
  [ print precision ( ( count patches with [ open-to-diving = true and bioc = "sand" ] / diving-patches-init-sand - 1 ) * 100 ) 0 ]
  [ print "n/a" ]

  print "variation of the total number of patches accessible for diving in the mud ecosystem"
  ifelse diving-patches-init-mud != 0
  [ print precision ( ( count patches with [ open-to-diving = true and bioc = "mud" ] / diving-patches-init-mud - 1 ) * 100 ) 0 ]
  [ print "n/a" ]

  print "variation of the total number of patches accessible for diving in the rock ecosystem"
  ifelse diving-patches-init-rock != 0
  [ print precision ( ( count patches with [ open-to-diving = true and bioc = "rock" ] / diving-patches-init-rock - 1 ) * 100 ) 0 ]
  [ print "n/a" ]

  print "variation of the total number of patches accessible for diving in the posidonia ecosystem"
  ifelse diving-patches-init-posi != 0
  [ print precision ( ( count patches with [ open-to-diving = true and bioc = "posi" ] / diving-patches-init-posi - 1 ) * 100 ) 0 ]
  [ print "n/a" ]

  print "variation of the total number of patches accessible for diving in the coralligenous ecosystem"
  ifelse diving-patches-init-coral != 0
  [ print precision ( ( count patches with [ open-to-diving = true and bioc = "coral" ] / diving-patches-init-coral - 1 ) * 100 ) 0 ]
  [ print "n/a" ]

end

;;;;;;;;;;;;;;;;;
;;;;REPORTERS;;;;
;;;;;;;;;;;;;;;;;

to-report divide [ gross tot ]

  let % []
  ( foreach gross tot [ [ ?1 ?2 ] -> ifelse ?2 = 0
    [ set % lput 0 % ]
    [ set % lput ( ?1 / ?2 ) % ] ] )
  report %

end

to-report P [ ratio Biom Q/Biom ]

  let sum-ratio map [ i -> sum i ] ( matrix:to-column-list ratio )
  let auto []
  ( foreach Biom Q/Biom sum-ratio [ [ ?1 ?2 ?3 ] -> set auto lput ( ?1 * ?2 * ( 1 - ?3 ) ) auto ] )
  report auto

end

to-report multiply [ ratio Q/Biom-matrix Biom-matrix ]

  let gross ( matrix:times-element-wise ratio Q/Biom-matrix Biom-matrix )
  report gross

end

to-report compare [ t1 t0 ]

  let % []
  ( foreach t1 t0 [ [ ?1 ?2 ] -> ifelse ?2 = 0
    [ set % lput 0 % ]
    [ set % lput precision ( ( ?1 - ?2 ) / ?2 * 100 ) 2 % ] ] )
  report %

end

to-report switch-columns [ items1 items2 data1 data2 ]

  ( foreach items1 items2 [ [ ?1 ?2 ] ->
    matrix:set-column data2 ?2 ( matrix:get-column data1 ?1 ) ] )
  report data2

end

to-report detail [ ratio sum-ratio gross ]

  let gross-detail []
  ( foreach ratio sum-ratio gross
    [ [ ?1 ?2 ?3 ] -> ifelse ?2 = 0
    [ set gross-detail lput 0 gross-detail ]
    [ set gross-detail lput ( ?1 / ?2 * ?3 ) gross-detail ] ] )
  report gross-detail

end

to-report update [ B-C detail-init ]

  let detail-inter []
  ( foreach B-C detail-init [ [ ?1 ?2 ] -> ifelse ?1 = 0
    [ set detail-inter lput ?1 detail-inter ]
    [ set detail-inter lput ?2 detail-inter ] ] )
  report detail-inter

end

to-report FPA-%

  report
  count patches with [ no-go-take = true ]
  /
  count patches with [ nat-rich-level >= 0 ] * 100

end
@#$#@#$#@
GRAPHICS-WINDOW
310
73
738
502
-1
-1
2.2
1
10
1
1
1
0
0
0
1
-95
95
-95
95
0
0
1
ticks
30.0

BUTTON
399
23
462
56
go
go
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

BUTTON
479
23
542
56
go
go
T
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

BUTTON
46
19
109
52
setup
setup
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

PLOT
56
1290
688
1583
undersea & fished biomass
NIL
NIL
0.0
10.0
0.0
10.0
true
true
"" ""
PENS
"undersea" 1.0 0 -10402772 true "" ""
"trawls" 1.0 0 -7171555 true "" ""
"lamparos" 1.0 0 -13210332 true "" ""
"tuna-seiner" 1.0 0 -15040220 true "" ""
"other-artisanal-fisheries" 1.0 0 -7500403 true "" ""

INPUTBOX
787
187
942
247
phyto-2100
-10.0
1
0
Number

TEXTBOX
789
99
939
141
assumption on phytoplankton biomass change by the end of the century in %
11
0.0
1

SWITCH
787
151
926
184
phyto-change?
phyto-change?
1
1
-1000

INPUTBOX
41
226
196
286
trawls-var
5.0
1
0
Number

INPUTBOX
42
293
197
353
lamparos-var
5.0
1
0
Number

INPUTBOX
42
359
197
419
tuna-seiner-var
5.0
1
0
Number

INPUTBOX
43
425
197
485
other-artisanal-fisheries-var
5.0
1
0
Number

SWITCH
29
177
213
210
fishing-change?
fishing-change?
1
1
-1000

BUTTON
561
23
638
56
go 2050
go-2050
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

OUTPUT
785
37
887
72
11

TEXTBOX
787
13
937
31
year
11
0.0
1

MONITOR
786
451
884
496
FPA-%
count patches with [ no-go-take = true ] / count patches with [ nat-rich-level >= 0 ] * 100
2
1
11

CHOOSER
786
400
925
445
zoning
zoning
"FPA -> 30%" "FPA -> 2%" "FPA -> 10%"
0

PLOT
34
633
631
939
sand
NIL
NIL
0.0
10.0
0.0
10.0
true
true
"" ""
PENS
"phytoplankton" 1.0 0 -16777216 true "" ""
"zooplankton" 1.0 0 -7500403 true "" ""
"detritus" 1.0 0 -2674135 true "" ""
"microphytobenthos" 1.0 0 -955883 true "" ""
"worms" 1.0 0 -6459832 true "" ""
"suprabenthos" 1.0 0 -1184463 true "" ""
"bivalves-and-gastropods" 1.0 0 -10899396 true "" ""
"benthic-invertebrates" 1.0 0 -13840069 true "" ""
"lobsters" 1.0 0 -14835848 true "" ""
"crabs" 1.0 0 -11221820 true "" ""
"echinoderms" 1.0 0 -13791810 true "" ""
"small-pelagic-fish" 1.0 0 -13345367 true "" ""
"soles" 1.0 0 -8630108 true "" ""
"fish-feeding-on-polychaetes-bivalve-and-gastropods" 1.0 0 -5825686 true "" ""
"octopuses" 1.0 0 -2064490 true "" ""
"cuttlefish-and-squids" 1.0 0 -16777216 true "" ""
"gilthead-seabream" 1.0 0 -16777216 true "" ""
"european-seabass" 1.0 0 -16777216 true "" ""
"red-mullets" 1.0 0 -16777216 true "" ""
"fish-feeding-on-benthic-crustaceans" 1.0 0 -16777216 true "" ""
"horse-mackerel" 1.0 0 -16777216 true "" ""
"mackerel" 1.0 0 -16777216 true "" ""
"blue-whiting" 1.0 0 -16777216 true "" ""
"hake" 1.0 0 -16777216 true "" ""
"atlantic-bluefin-tuna" 1.0 0 -16777216 true "" ""
"other-large-pelagic-fishes" 1.0 0 -16777216 true "" ""
"anglerfish" 1.0 0 -16777216 true "" ""
"rays-and-skates" 1.0 0 -16777216 true "" ""
"sharks" 1.0 0 -16777216 true "" ""
"baleen-whales" 1.0 0 -16777216 true "" ""
"dolphins" 1.0 0 -16777216 true "" ""
"trawls" 1.0 0 -16777216 true "" ""
"lamparos" 1.0 0 -16777216 true "" ""
"tuna-seiner" 1.0 0 -16777216 true "" ""
"other-artisanal-fisheries" 1.0 0 -16777216 true "" ""

PLOT
54
683
699
970
rock
NIL
NIL
0.0
10.0
0.0
10.0
true
true
"" ""
PENS
"phytoplankton" 1.0 0 -16777216 true "" ""
"zooplankton" 1.0 0 -7500403 true "" ""
"detritus" 1.0 0 -2674135 true "" ""
"benthic-macrophytes" 1.0 0 -955883 true "" ""
"worms" 1.0 0 -6459832 true "" ""
"suprabenthos" 1.0 0 -1184463 true "" ""
"white-gorgonian" 1.0 0 -10899396 true "" ""
"benthic-invertebrates" 1.0 0 -13840069 true "" ""
"echinoderms" 1.0 0 -14835848 true "" ""
"octopuses" 1.0 0 -11221820 true "" ""
"bivalves-and-gastropods" 1.0 0 -13791810 true "" ""
"salema" 1.0 0 -13345367 true "" ""
"fish-feeding-on-polychaetes-bivalve-and-gastropods" 1.0 0 -8630108 true "" ""
"gilthead-seabream" 1.0 0 -5825686 true "" ""
"european-conger" 1.0 0 -2064490 true "" ""
"european-seabass" 1.0 0 -16777216 true "" ""
"scorpion-fish" 1.0 0 -16777216 true "" ""
"picarel" 1.0 0 -16777216 true "" ""
"grouper/dentex" 1.0 0 -16777216 true "" ""
"trawls" 1.0 0 -16777216 true "" ""
"lamparos" 1.0 0 -16777216 true "" ""
"tuna-seiner" 1.0 0 -16777216 true "" ""
"other-artisanal-fisheries" 1.0 0 -16777216 true "" ""

PLOT
95
746
738
1065
posi
NIL
NIL
0.0
10.0
0.0
10.0
true
true
"" ""
PENS
"phytoplankton" 1.0 0 -16777216 true "" ""
"zooplankton" 1.0 0 -7500403 true "" ""
"detritus" 1.0 0 -2674135 true "" ""
"posidonia" 1.0 0 -955883 true "" ""
"bivalves-and-gastropods" 1.0 0 -6459832 true "" ""
"suprabenthos" 1.0 0 -1184463 true "" ""
"salema" 1.0 0 -10899396 true "" ""
"echinoderms" 1.0 0 -13840069 true "" ""
"crabs" 1.0 0 -14835848 true "" ""
"fish-feeding-on-polychaetes-bivalve-and-gastropods" 1.0 0 -11221820 true "" ""
"gilthead-seabream" 1.0 0 -13791810 true "" ""
"octopuses" 1.0 0 -13345367 true "" ""
"cuttlefish-and-squids" 1.0 0 -8630108 true "" ""
"european-conger" 1.0 0 -5825686 true "" ""
"scorpion-fish" 1.0 0 -2064490 true "" ""
"european-seabass" 1.0 0 -16777216 true "" ""
"picarel" 1.0 0 -16777216 true "" ""
"trawls" 1.0 0 -16777216 true "" ""
"lamparos" 1.0 0 -16777216 true "" ""
"tuna-seiner" 1.0 0 -16777216 true "" ""
"other-artisanal-fisheries" 1.0 0 -16777216 true "" ""

PLOT
136
804
776
1146
coral
NIL
NIL
0.0
10.0
0.0
10.0
true
true
"" ""
PENS
"phytoplankton" 1.0 0 -16777216 true "" ""
"zooplankton" 1.0 0 -7500403 true "" ""
"detritus" 1.0 0 -2674135 true "" ""
"benthic-macrophytes" 1.0 0 -955883 true "" ""
"worms" 1.0 0 -6459832 true "" ""
"benthic-invertebrates" 1.0 0 -1184463 true "" ""
"echinoderms" 1.0 0 -10899396 true "" ""
"bivalves-and-gastropods" 1.0 0 -13840069 true "" ""
"planctonophagous-fish" 1.0 0 -14835848 true "" ""
"fish-feeding-on-polychaetes-bivalve-and-gastropods" 1.0 0 -11221820 true "" ""
"octopuses" 1.0 0 -13791810 true "" ""
"european-seabass" 1.0 0 -13345367 true "" ""
"scorpion-fish" 1.0 0 -8630108 true "" ""
"suprabenthos" 1.0 0 -5825686 true "" ""
"lobsters" 1.0 0 -2064490 true "" ""
"trawls" 1.0 0 -16777216 true "" ""
"lamparos" 1.0 0 -16777216 true "" ""
"tuna-seiner" 1.0 0 -16777216 true "" ""
"other-artisanal-fisheries" 1.0 0 -16777216 true "" ""

SWITCH
787
361
966
394
fully-protected-areas?
fully-protected-areas?
1
1
-1000

PLOT
817
1121
1158
1335
densi-reefs
NIL
NIL
0.0
10.0
0.0
10.0
true
true
"" ""
PENS
"phytoplankton" 1.0 0 -16777216 true "" ""
"zooplankton" 1.0 0 -7500403 true "" ""
"detritus" 1.0 0 -2674135 true "" ""
"benthic-macrophytes" 1.0 0 -955883 true "" ""
"worms" 1.0 0 -6459832 true "" ""
"suprabenthos" 1.0 0 -1184463 true "" ""
"white-gorgonian" 1.0 0 -10899396 true "" ""
"benthic-invertebrates" 1.0 0 -13840069 true "" ""
"echinoderms" 1.0 0 -14835848 true "" ""
"octopuses" 1.0 0 -11221820 true "" ""
"bivalves-and-gastropods" 1.0 0 -13791810 true "" ""
"salema" 1.0 0 -13345367 true "" ""
"fish-feeding-on-polychaetes-bivalve-and-gastropods" 1.0 0 -8630108 true "" ""
"gilthead-seabream" 1.0 0 -5825686 true "" ""
"european-conger" 1.0 0 -2064490 true "" ""
"european-seabass" 1.0 0 -16777216 true "" ""
"scorpion-fish" 1.0 0 -16777216 true "" ""
"picarel" 1.0 0 -16777216 true "" ""
"grouper/dentex" 1.0 0 -16777216 true "" ""
"trawls" 1.0 0 -16777216 true "" ""
"lamparos" 1.0 0 -16777216 true "" ""
"tuna-seiner" 1.0 0 -16777216 true "" ""
"other-artisanal-fisheries" 1.0 0 -16777216 true "" ""

PLOT
854
880
1206
1114
repop-reefs
NIL
NIL
0.0
10.0
0.0
10.0
true
true
"" ""
PENS
"phytoplankton" 1.0 0 -16777216 true "" ""
"zooplankton" 1.0 0 -7500403 true "" ""
"detritus" 1.0 0 -2674135 true "" ""
"benthic-macrophytes" 1.0 0 -955883 true "" ""
"worms" 1.0 0 -6459832 true "" ""
"suprabenthos" 1.0 0 -1184463 true "" ""
"white-gorgonian" 1.0 0 -10899396 true "" ""
"benthic-invertebrates" 1.0 0 -13840069 true "" ""
"echinoderms" 1.0 0 -14835848 true "" ""
"octopuses" 1.0 0 -11221820 true "" ""
"bivalves-and-gastropods" 1.0 0 -13791810 true "" ""
"salema" 1.0 0 -13345367 true "" ""
"fish-feeding-on-polychaetes-bivalve-and-gastropods" 1.0 0 -8630108 true "" ""
"gilthead-seabream" 1.0 0 -5825686 true "" ""
"european-conger" 1.0 0 -2064490 true "" ""
"european-seabass" 1.0 0 -16777216 true "" ""
"scorpion-fish" 1.0 0 -16777216 true "" ""
"picarel" 1.0 0 -16777216 true "" ""
"grouper/dentex" 1.0 0 -16777216 true "" ""
"trawls" 1.0 0 -16777216 true "" ""
"lamparos" 1.0 0 -16777216 true "" ""
"tuna-seiner" 1.0 0 -16777216 true "" ""
"other-artisanal-fisheries" 1.0 0 -16777216 true "" ""

PLOT
878
622
1249
873
grow-reefs
NIL
NIL
0.0
10.0
0.0
10.0
true
true
"" ""
PENS
"phytoplankton" 1.0 0 -16777216 true "" ""
"zooplankton" 1.0 0 -7500403 true "" ""
"detritus" 1.0 0 -2674135 true "" ""
"benthic-macrophytes" 1.0 0 -955883 true "" ""
"worms" 1.0 0 -6459832 true "" ""
"suprabenthos" 1.0 0 -1184463 true "" ""
"white-gorgonian" 1.0 0 -10899396 true "" ""
"benthic-invertebrates" 1.0 0 -13840069 true "" ""
"echinoderms" 1.0 0 -14835848 true "" ""
"octopuses" 1.0 0 -11221820 true "" ""
"bivalves-and-gastropods" 1.0 0 -13791810 true "" ""
"salema" 1.0 0 -13345367 true "" ""
"fish-feeding-on-polychaetes-bivalve-and-gastropods" 1.0 0 -8630108 true "" ""
"gilthead-seabream" 1.0 0 -5825686 true "" ""
"european-conger" 1.0 0 -2064490 true "" ""
"european-seabass" 1.0 0 -16777216 true "" ""
"scorpion-fish" 1.0 0 -16777216 true "" ""
"picarel" 1.0 0 -16777216 true "" ""
"grouper/dentex" 1.0 0 -16777216 true "" ""
"trawls" 1.0 0 -16777216 true "" ""
"lamparos" 1.0 0 -16777216 true "" ""
"tuna-seiner" 1.0 0 -16777216 true "" ""
"other-artisanal-fisheries" 1.0 0 -16777216 true "" ""

PLOT
172
887
808
1226
mud
NIL
NIL
0.0
10.0
0.0
10.0
true
true
"" ""
PENS
"phytoplankton" 1.0 0 -16777216 true "" ""
"zooplankton" 1.0 0 -7500403 true "" ""
"detritus" 1.0 0 -2674135 true "" ""
"microphytobenthos" 1.0 0 -955883 true "" ""
"worms" 1.0 0 -6459832 true "" ""
"suprabenthos" 1.0 0 -1184463 true "" ""
"bivalves-and-gastropods" 1.0 0 -10899396 true "" ""
"benthic-invertebrates" 1.0 0 -13840069 true "" ""
"lobsters" 1.0 0 -14835848 true "" ""
"crabs" 1.0 0 -11221820 true "" ""
"echinoderms" 1.0 0 -13791810 true "" ""
"small-pelagic-fish" 1.0 0 -13345367 true "" ""
"soles" 1.0 0 -8630108 true "" ""
"fish-feeding-on-polychaetes-bivalve-and-gastropods" 1.0 0 -5825686 true "" ""
"octopuses" 1.0 0 -2064490 true "" ""
"cuttlefish-and-squids" 1.0 0 -16777216 true "" ""
"gilthead-seabream" 1.0 0 -16777216 true "" ""
"european-seabass" 1.0 0 -16777216 true "" ""
"red-mullets" 1.0 0 -16777216 true "" ""
"fish-feeding-on-benthic-crustaceans" 1.0 0 -16777216 true "" ""
"horse-mackerel" 1.0 0 -16777216 true "" ""
"mackerel" 1.0 0 -16777216 true "" ""
"blue-whiting" 1.0 0 -16777216 true "" ""
"hake" 1.0 0 -16777216 true "" ""
"atlantic-bluefin-tuna" 1.0 0 -16777216 true "" ""
"other-large-pelagic-fishes" 1.0 0 -16777216 true "" ""
"anglerfish" 1.0 0 -16777216 true "" ""
"rays-and-skates" 1.0 0 -16777216 true "" ""
"sharks" 1.0 0 -16777216 true "" ""
"baleen-whales" 1.0 0 -16777216 true "" ""
"dolphins" 1.0 0 -16777216 true "" ""
"trawls" 1.0 0 -16777216 true "" ""
"lamparos" 1.0 0 -16777216 true "" ""
"tuna-seiner" 1.0 0 -16777216 true "" ""
"other-artisanal-fisheries" 1.0 0 -16777216 true "" ""

CHOOSER
127
20
265
65
scenario
scenario
"S1" "S2" "S3" "no"
3

TEXTBOX
785
265
935
349
three pre-encoded 30%/2%/10% fully protected areas shares\n(extensions in 2020/2025/2030)
11
0.0
1

TEXTBOX
39
102
183
174
assumptions on fishing effort change for fishing gears at the beginning of the simulation (2019)\n
11
0.0
1

@#$#@#$#@
## WHAT IS IT?

This model was developed as a part of a project addressing trade-offs between ecosystem services in a french marine protected area with science-policy prospective experiments. It is made to explore some effects of climate change, fishing and management on the biomass of each (group of) species, and specifically on the distribution of biomass between each group of an ecosystem. Then simulation results can be used to build ecosystem services indicators. It consists in a simplified ecosystem-based representation of the social-ecological system of our case study, mapping 4 major ecosystems representative for the area: "sand&mud" (31 species), "rock" (18 species), "posidonia" (17 species), "coralligenous" (15 species). Here species are represented in aggregate form (biomass density) and linked together with diet ratios.

## HOW IT WORKS

To simulate ecosystem dynamics, we use the food-webs corresponding to the selected ecosystems as transmission chains for the following types of controlling factors: bottom-up control (climate, management), top-down control (fisheries, management). Climate change is represented through the possible variation of primary production (phytoplankton biomass). Fishing is represented through four fishing gears whose effort can evolve under management measures. Management is represented through fully protected areas development, facilities planning (especially artificial reefs) and ecological engineering (reintroduction of species). For each (group of) species, biomass variation results from the equal combination of two potential drivers on a yearly basis: the abundance of prey (bottom-up control, positive feedback) and the abundance of predators (top-down control, negative feedback). The food-web model is located at the cell level with previous year's outputs as input data for each new year. Other human and non-human agents are also represented at the cell level. At this stage we model temporal dynamics but we lack spatial dynamics such as adaptive behaviors of human and non-human agents resulting in transfer effects between sites or towards new sites. For now, interactions between agents are mostly made of spatial-temporal co-occurrence with restricted mobility. In this respect, the model simulates the variation of any group of species in terms of biomass density in case of a change in primary production, fishing effort, artificial reefs planning or reintroduction of species. In accordance with the project specifications, the size of each patch is related to the average size of an artificial reef village (0,25 km2), and simulations run by 2050 with an annual time step.

## HOW TO USE IT

From the Interface tab the model can be launched following one of three scenarios developed in the context of the project or no specific scenario but any assumption on phytoplankton biomass change by 2100 or/and fishing effort change from 2019. An additional assumption on fully protected areas development can be made by choosing one of the three pre-encoded 30%/2%/10% shares (progressive extensions in 2020/2025/2030 and distinct rules of attribution). Any other change requires to enter the Code tab. A list of output indicators is automatically printed in the Command center at the end of any "go-2050" simulation. You can print the same list of indicators at any other time of a different simulation starting by 2018 using the "print-results" command in the Command center. Plots show the evolution of biomass density for each species of each ecosystem or of locations concerned with artificial reefs management, and for fished biomass by fishing gear.

## THINGS TO NOTICE

In case of no change in initial conditions the model exhibits a steady-state dynamic. This choice comes from our main source of modelling principles and data (Ewe Gulf of Lions, see "related models" below) that represents an average annual situation over the decade 2000 - 2009 with no clue on intermediary dynamics.
This version of the model is deterministic.

## THINGS TO TRY

- Explore possible effects of a change in primary production on the distribution of biomass between ecosystem compartiments ("assumption on phytoplankton biomass change by the end of the century in %").
- Explore possible effects of a change in fishing effort on the distribution of biomass between ecosystem compartiments ("assumptions on fishing effort change for fishing gears at the beginning of the simulation (2019)").
- Explore possible effects of a fully protected areas development on the distribution of biomass between ecosystem compartiments ("three pre-encoded 30%/2%/10% fully protected areas shares").

## EXTENDING THE MODEL

- Introduce adaptive behaviors of human and non-human agents resulting in transfer effects between sites or towards new sites since fully protected areas development also generates changes in their surroundings: deplacement of the fishing effort and spillover effects of marine organisms.
- Introduce stochasticity.
- Try to adapt the model to another case study since functions were written in order to help generalize operations regardless of the size of input data i.e. the number of interacting species in the ecosystem.

## NETLOGO FEATURES

- This model uses gis, csv and matrix extensions.
- At this stage, agents are patches that communicate with eachother in only two cases: the allocation rule of fully protected areas and floating wind turbines. 

## RELATED MODELS

The food-web modelling is freely adapted from the Ecopath models and scientific publications consulted for the project (modelling features and data).
https://ecopath.org/
Bănaru, D. et al. Trophic structure in the Gulf of Lions marine ecosystem (north-western Mediterranean Sea) and fishing impacts. Journal of Marine Systems 111, 45-68 (2013).
Corrales, X. et al. Ecosystem structure and fishing impacts in the northwestern Mediterranean Sea using a food web model within a comparative approach. Journal of Marine Systems 148, 183-199 (2015).

## CREDITS AND REFERENCES

If you mention this model in a publication, we ask that you include the citation below:
Mosseri, E. (2019). Ecosystem-based modelling of a marine protected area. https://github.com/elsamosseri/SAFRAN. Centre international de recherche sur l’environnement et le développement (CIRED) - UMR 8568 (CNRS-ENPC-EHESS-Cirad-AgroParisTech).

This code is associated with a forthcoming publication:
Boemare, C., Mosseri, E., Agin, G., Bramanti, L., Certain, R., Claudet, J., Guizien, K., Jabouin, C., Jarraya, M., Lagurgue, X., Lenfant, P., Levrel, H., Michel, C., Musard, O.
Hybridizing research and decision-making: a path toward sustainability in marine spaces.
Submitted January 2022 to Nature Sustainability.

Code by Elsa Mosseri
https://www.centre-cired.fr/elsa-mosseri/
As part of the SAFRAN project - Scenarios for the development of the coastal fringe and the marine environment: an integrated prospective of the social-ecological system of the Gulf of Lions’ Marine Natural Park.
Project coordinated by Catherine Boemare
https://www.centre-cired.fr/catherine-boemare/
Project financed by the Fondation de France and the Agence des Aires Marines Protégées. 
https://www.fondationdefrance.org/fr
https://ofb.gouv.fr/

                      *******************

Copyright or © or Copr. CNRS 2019. 
Main Contributor (2019) : Elsa Mosseri / mosseri[at]centre-cired.fr

This software is a computer program whose purpose is to explore the 
distribution of biomass between each group of marine ecosystems under 
climate, fishing and management constraints.

This software is governed by the CeCILL-B license under French law and
abiding by the rules of distribution of free software.  You can  use, 
modify and/ or redistribute the software under the terms of the CeCILL-B
license as circulated by CEA, CNRS and INRIA at the following URL
"http://www.cecill.info". 

As a counterpart to the access to the source code and  rights to copy,
modify and redistribute granted by the license, users are provided only
with a limited warranty  and the software's author,  the holder of the
economic rights,  and the successive licensors  have only  limited
liability. 

In this respect, the user's attention is drawn to the risks associated
with loading,  using,  modifying and/or developing or reproducing the
software by the user in light of its specific status of free software,
that may mean  that it is complicated to manipulate,  and  that  also
therefore means  that it is reserved for developers  and  experienced
professionals having in-depth computer knowledge. Users are therefore
encouraged to load and test the software's suitability as regards their
requirements in conditions enabling the security of their systems and/or 
data to be ensured and,  more generally, to use and operate it in the 
same conditions as regards security. 

The fact that you are presently reading this means that you have had
knowledge of the CeCILL-B license and that you accept its terms.
@#$#@#$#@
default
true
0
Polygon -7500403 true true 150 5 40 250 150 205 260 250

airplane
true
0
Polygon -7500403 true true 150 0 135 15 120 60 120 105 15 165 15 195 120 180 135 240 105 270 120 285 150 270 180 285 210 270 165 240 180 180 285 195 285 165 180 105 180 60 165 15

arrow
true
0
Polygon -7500403 true true 150 0 0 150 105 150 105 293 195 293 195 150 300 150

box
false
0
Polygon -7500403 true true 150 285 285 225 285 75 150 135
Polygon -7500403 true true 150 135 15 75 150 15 285 75
Polygon -7500403 true true 15 75 15 225 150 285 150 135
Line -16777216 false 150 285 150 135
Line -16777216 false 150 135 15 75
Line -16777216 false 150 135 285 75

bug
true
0
Circle -7500403 true true 96 182 108
Circle -7500403 true true 110 127 80
Circle -7500403 true true 110 75 80
Line -7500403 true 150 100 80 30
Line -7500403 true 150 100 220 30

butterfly
true
0
Polygon -7500403 true true 150 165 209 199 225 225 225 255 195 270 165 255 150 240
Polygon -7500403 true true 150 165 89 198 75 225 75 255 105 270 135 255 150 240
Polygon -7500403 true true 139 148 100 105 55 90 25 90 10 105 10 135 25 180 40 195 85 194 139 163
Polygon -7500403 true true 162 150 200 105 245 90 275 90 290 105 290 135 275 180 260 195 215 195 162 165
Polygon -16777216 true false 150 255 135 225 120 150 135 120 150 105 165 120 180 150 165 225
Circle -16777216 true false 135 90 30
Line -16777216 false 150 105 195 60
Line -16777216 false 150 105 105 60

car
false
0
Polygon -7500403 true true 300 180 279 164 261 144 240 135 226 132 213 106 203 84 185 63 159 50 135 50 75 60 0 150 0 165 0 225 300 225 300 180
Circle -16777216 true false 180 180 90
Circle -16777216 true false 30 180 90
Polygon -16777216 true false 162 80 132 78 134 135 209 135 194 105 189 96 180 89
Circle -7500403 true true 47 195 58
Circle -7500403 true true 195 195 58

circle
false
0
Circle -7500403 true true 0 0 300

circle 2
false
0
Circle -7500403 true true 0 0 300
Circle -16777216 true false 30 30 240

contour
false
0
Rectangle -16777216 false false 0 0 300 300

cow
false
0
Polygon -7500403 true true 200 193 197 249 179 249 177 196 166 187 140 189 93 191 78 179 72 211 49 209 48 181 37 149 25 120 25 89 45 72 103 84 179 75 198 76 252 64 272 81 293 103 285 121 255 121 242 118 224 167
Polygon -7500403 true true 73 210 86 251 62 249 48 208
Polygon -7500403 true true 25 114 16 195 9 204 23 213 25 200 39 123

cylinder
false
0
Circle -7500403 true true 0 0 300

dot
false
0
Circle -7500403 true true 90 90 120

face happy
false
0
Circle -7500403 true true 8 8 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Polygon -16777216 true false 150 255 90 239 62 213 47 191 67 179 90 203 109 218 150 225 192 218 210 203 227 181 251 194 236 217 212 240

face neutral
false
0
Circle -7500403 true true 8 7 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Rectangle -16777216 true false 60 195 240 225

face sad
false
0
Circle -7500403 true true 8 8 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Polygon -16777216 true false 150 168 90 184 62 210 47 232 67 244 90 220 109 205 150 198 192 205 210 220 227 242 251 229 236 206 212 183

fish
false
0
Polygon -1 true false 44 131 21 87 15 86 0 120 15 150 0 180 13 214 20 212 45 166
Polygon -1 true false 135 195 119 235 95 218 76 210 46 204 60 165
Polygon -1 true false 75 45 83 77 71 103 86 114 166 78 135 60
Polygon -7500403 true true 30 136 151 77 226 81 280 119 292 146 292 160 287 170 270 195 195 210 151 212 30 166
Circle -16777216 true false 215 106 30

flag
false
0
Rectangle -7500403 true true 60 15 75 300
Polygon -7500403 true true 90 150 270 90 90 30
Line -7500403 true 75 135 90 135
Line -7500403 true 75 45 90 45

flower
false
0
Polygon -10899396 true false 135 120 165 165 180 210 180 240 150 300 165 300 195 240 195 195 165 135
Circle -7500403 true true 85 132 38
Circle -7500403 true true 130 147 38
Circle -7500403 true true 192 85 38
Circle -7500403 true true 85 40 38
Circle -7500403 true true 177 40 38
Circle -7500403 true true 177 132 38
Circle -7500403 true true 70 85 38
Circle -7500403 true true 130 25 38
Circle -7500403 true true 96 51 108
Circle -16777216 true false 113 68 74
Polygon -10899396 true false 189 233 219 188 249 173 279 188 234 218
Polygon -10899396 true false 180 255 150 210 105 210 75 240 135 240

house
false
0
Rectangle -7500403 true true 45 120 255 285
Rectangle -16777216 true false 120 210 180 285
Polygon -7500403 true true 15 120 150 15 285 120
Line -16777216 false 30 120 270 120

leaf
false
0
Polygon -7500403 true true 150 210 135 195 120 210 60 210 30 195 60 180 60 165 15 135 30 120 15 105 40 104 45 90 60 90 90 105 105 120 120 120 105 60 120 60 135 30 150 15 165 30 180 60 195 60 180 120 195 120 210 105 240 90 255 90 263 104 285 105 270 120 285 135 240 165 240 180 270 195 240 210 180 210 165 195
Polygon -7500403 true true 135 195 135 240 120 255 105 255 105 285 135 285 165 240 165 195

line
true
0
Line -7500403 true 150 0 150 300

line half
true
0
Line -7500403 true 150 0 150 150

pentagon
false
0
Polygon -7500403 true true 150 15 15 120 60 285 240 285 285 120

person
false
0
Circle -7500403 true true 110 5 80
Polygon -7500403 true true 105 90 120 195 90 285 105 300 135 300 150 225 165 300 195 300 210 285 180 195 195 90
Rectangle -7500403 true true 127 79 172 94
Polygon -7500403 true true 195 90 240 150 225 180 165 105
Polygon -7500403 true true 105 90 60 150 75 180 135 105

plant
false
0
Rectangle -7500403 true true 135 90 165 300
Polygon -7500403 true true 135 255 90 210 45 195 75 255 135 285
Polygon -7500403 true true 165 255 210 210 255 195 225 255 165 285
Polygon -7500403 true true 135 180 90 135 45 120 75 180 135 210
Polygon -7500403 true true 165 180 165 210 225 180 255 120 210 135
Polygon -7500403 true true 135 105 90 60 45 45 75 105 135 135
Polygon -7500403 true true 165 105 165 135 225 105 255 45 210 60
Polygon -7500403 true true 135 90 120 45 150 15 180 45 165 90

sheep
false
15
Circle -1 true true 203 65 88
Circle -1 true true 70 65 162
Circle -1 true true 150 105 120
Polygon -7500403 true false 218 120 240 165 255 165 278 120
Circle -7500403 true false 214 72 67
Rectangle -1 true true 164 223 179 298
Polygon -1 true true 45 285 30 285 30 240 15 195 45 210
Circle -1 true true 3 83 150
Rectangle -1 true true 65 221 80 296
Polygon -1 true true 195 285 210 285 210 240 240 210 195 210
Polygon -7500403 true false 276 85 285 105 302 99 294 83
Polygon -7500403 true false 219 85 210 105 193 99 201 83

square
false
0
Rectangle -7500403 true true 30 30 270 270

square 2
false
0
Rectangle -7500403 true true 30 30 270 270
Rectangle -16777216 true false 60 60 240 240

square 3
false
14
Rectangle -7500403 true false 30 30 270 270
Rectangle -7500403 false false 75 60 210 165

star
false
0
Polygon -7500403 true true 151 1 185 108 298 108 207 175 242 282 151 216 59 282 94 175 3 108 116 108

target
false
0
Circle -7500403 true true 0 0 300
Circle -16777216 true false 30 30 240
Circle -7500403 true true 60 60 180
Circle -16777216 true false 90 90 120
Circle -7500403 true true 120 120 60

tree
false
0
Circle -7500403 true true 118 3 94
Rectangle -6459832 true false 120 195 180 300
Circle -7500403 true true 65 21 108
Circle -7500403 true true 116 41 127
Circle -7500403 true true 45 90 120
Circle -7500403 true true 104 74 152

triangle
false
0
Polygon -7500403 true true 150 30 15 255 285 255

triangle 2
false
0
Polygon -7500403 true true 150 30 15 255 285 255
Polygon -16777216 true false 151 99 225 223 75 224

truck
false
0
Rectangle -7500403 true true 4 45 195 187
Polygon -7500403 true true 296 193 296 150 259 134 244 104 208 104 207 194
Rectangle -1 true false 195 60 195 105
Polygon -16777216 true false 238 112 252 141 219 141 218 112
Circle -16777216 true false 234 174 42
Rectangle -7500403 true true 181 185 214 194
Circle -16777216 true false 144 174 42
Circle -16777216 true false 24 174 42
Circle -7500403 false true 24 174 42
Circle -7500403 false true 144 174 42
Circle -7500403 false true 234 174 42

turtle
true
0
Polygon -10899396 true false 215 204 240 233 246 254 228 266 215 252 193 210
Polygon -10899396 true false 195 90 225 75 245 75 260 89 269 108 261 124 240 105 225 105 210 105
Polygon -10899396 true false 105 90 75 75 55 75 40 89 31 108 39 124 60 105 75 105 90 105
Polygon -10899396 true false 132 85 134 64 107 51 108 17 150 2 192 18 192 52 169 65 172 87
Polygon -10899396 true false 85 204 60 233 54 254 72 266 85 252 107 210
Polygon -7500403 true true 119 75 179 75 209 101 224 135 220 225 175 261 128 261 81 224 74 135 88 99

wheel
false
0
Circle -7500403 true true 3 3 294
Circle -16777216 true false 30 30 240
Line -7500403 true 150 285 150 15
Line -7500403 true 15 150 285 150
Circle -7500403 true true 120 120 60
Line -7500403 true 216 40 79 269
Line -7500403 true 40 84 269 221
Line -7500403 true 40 216 269 79
Line -7500403 true 84 40 221 269

wolf
false
0
Polygon -16777216 true false 253 133 245 131 245 133
Polygon -7500403 true true 2 194 13 197 30 191 38 193 38 205 20 226 20 257 27 265 38 266 40 260 31 253 31 230 60 206 68 198 75 209 66 228 65 243 82 261 84 268 100 267 103 261 77 239 79 231 100 207 98 196 119 201 143 202 160 195 166 210 172 213 173 238 167 251 160 248 154 265 169 264 178 247 186 240 198 260 200 271 217 271 219 262 207 258 195 230 192 198 210 184 227 164 242 144 259 145 284 151 277 141 293 140 299 134 297 127 273 119 270 105
Polygon -7500403 true true -1 195 14 180 36 166 40 153 53 140 82 131 134 133 159 126 188 115 227 108 236 102 238 98 268 86 269 92 281 87 269 103 269 113

x
false
0
Polygon -7500403 true true 270 75 225 30 30 225 75 270
Polygon -7500403 true true 30 75 75 30 270 225 225 270
@#$#@#$#@
NetLogo 6.2.0
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
<experiments>
  <experiment name="experiment" repetitions="1" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="50"/>
    <metric>precision ( mean [ ( sum B / sum B-2018 - 1 ) * 100 ] of patches with  [active and sum B-2018 &gt; 0 ] ) 2</metric>
    <enumeratedValueSet variable="phyto-2100">
      <value value="0"/>
      <value value="-10"/>
      <value value="-20"/>
      <value value="-50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="pecheurs-var">
      <value value="0"/>
      <value value="5"/>
      <value value="10"/>
      <value value="50"/>
    </enumeratedValueSet>
  </experiment>
</experiments>
@#$#@#$#@
@#$#@#$#@
default
0.0
-0.2 0 0.0 1.0
0.0 1 1.0 0.0
0.2 0 0.0 1.0
link direction
true
0
Line -7500403 true 150 150 90 180
Line -7500403 true 150 150 210 180
@#$#@#$#@
0
@#$#@#$#@
