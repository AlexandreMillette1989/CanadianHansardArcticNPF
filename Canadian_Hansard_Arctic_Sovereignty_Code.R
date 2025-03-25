# Install libraries (skip if you already have those libraries installed) / run line 2-5
install.packages("quanteda")
install.packages("factoextra")
install.packages("ade4")
install.packages("rstatix")

# Run line 9 to line 248
# Import libraries
library(quanteda)
library(factoextra)
library(ade4)
library(rstatix)

# Import dataset
base = readRDS(file.choose())

# Tokenize + lower all characters 
# Clean punct, symbols, numbers, stopwords
baseTokens = tokens_remove(tokens(tolower(base$intervention), remove_punct = TRUE, remove_symbols = TRUE, remove_numbers = TRUE), stopwords("english"))

# create variable n words
base$words = ntoken(baseTokens)

# create custom dictionary with our themes
custom_dictionary = dictionary(list(
  environment_climate = c("air",
                          "animal*",
                          "biodiversity",
                          "carbon",
                          "change",
                          "changes",
                          "climate",
                          "coast*",
                          "conservation",
                          "ecolog*",
                          "ecosystem*",
                          "endangered",
                          "environment*",
                          "fauna",
                          "fish",
                          "fishery",
                          "fishes",
                          "fishing",
                          "footprint*",
                          "ghg",
                          "greenhouse",
                          "ice",
                          "lake",
                          "lakes",
                          "marine",
                          "melting",
                          "nature",
                          "ocean*",
                          "sea",
                          "seas",
                          "species",
                          "temperature*",
                          "traffic",
                          "transition",
                          "water",
                          "waters",
                          "waterway*",
                          "whale*",
                          "wildlife*"),
  military_security = c("admiral*",
                        "aggressi*",
                        "aircraft*",
                        "airfield*",
                        "alert",
                        "allies",
                        "armament*",
                        "armed",
                        "arsenal",
                        "ballistic",
                        "bases",
                        "battle*",
                        "belligeren*",
                        "border*",
                        "china",
                        "chinese",
                        "coast",
                        "combat",
                        "conflict*",
                        "continental",
                        "crime*",
                        "defence",
                        "destabiliz*",
                        "drone*",
                        "equipment*",
                        "extraterritorial",
                        "fleet",
                        "forces",
                        "guard",
                        "hypersonic",
                        "icebreaker*",
                        "incursion*",
                        "intelligence",
                        "international",
                        "invasion",
                        "jet*",
                        "kremlin",
                        "labrov",
                        "marine",
                        "militar*",
                        "missile*",
                        "nato",
                        "naval",
                        "navy",
                        "norad",
                        "northwest",
                        "nuclear",
                        "officer*",
                        "passage",
                        "patrol*",
                        "peace",
                        "peacekeeping",
                        "plane*",
                        "port*",
                        "presence",
                        "protect*",
                        "putin",
                        "radar*",
                        "ranger*",
                        "remilitarization",
                        "rescue",
                        "reserve",
                        "resolute",
                        "russia*",
                        "safe",
                        "safety",
                        "satellite*",
                        "secure",
                        "security",
                        "ship",
                        "shipbuilding",
                        "ships",
                        "shore*",
                        "soldier*",
                        "stability",
                        "strateg*",
                        "submarine*",
                        "surveillance",
                        "tactic*",
                        "target*",
                        "threat*",
                        "torpido*",
                        "troop*",
                        "ukraine",
                        "vessels",
                        "war",
                        "warfare",
                        "warhead",
                        "warheads",
                        "warning",
                        "wars",
                        "warship*"),
  indigenous_peoples = c("abitibiwinnik",
                         "aboriginal",
                         "akeeagok",
                         "algonquin",
                         "anishinabe",
                         "atikamekw",
                         "carmacks",
                         "colonial",
                         "communities",
                         "cree",
                         "dogrib",
                         "gwich'in",
                         "gwitchin",
                         "indigenous",
                         "innu",
                         "inuit",
                         "inuinnaqtun",
                         "inuk",
                         "inuktitut",
                         "inuvik",
                         "itk",
                         "lyackson",
                         "listuguj",
                         "katzie",
                         "kinanaskomtinawaw",
                         "kwantlen",
                         "manawan",
                         "matsqui",
                         "métis",
                         "mi'kmaq",
                         "nations",
                         "nanisivik",
                         "nasittuq",
                         "northerner*",
                         "nunavummiut",    
                         "nunavut",
                         "nunavik",
                         "okanagan",
                         "packchi-wanis",
                         "patriarchal",
                         "pauingassi",
                         "peguis",
                         "pikogan",
                         "reconciliation",
                         "saanich",
                         "semiahmoo",
                         "shamattawa",
                         "snaw-naw-as",
                         "snuneymuxw",
                         "stz'uminus",
                         "tadoule",
                         "territor*",
                         "tlingit",
                         "uashat-maliotenam",
                         "uqaqtittiji",
                         "wela'lin",
                         "wet'suwet'en",
                         "wiiliideh"),
  economy_resources = c("development",
                        "econom*",
                        "employ*",
                        "energ*",
                        "fuel",
                        "fossil",
                        "gas",
                        "industr*",
                        "infrastructure",
                        "invest*",
                        "job*",
                        "mineral*",
                        "offshore*",
                        "oil",
                        "petroleum",
                        "producer*",
                        "resource*",
                        "seabed",
                        "shelf",
                        "shipping",
                        "spending",
                        "supplies",
                        "tanker*",
                        "trade",
                        "work")))

# create document feature matrix from Tokens
dfmToken = dfm(baseTokens)

# create themes dataset
themes = convert(dfm_lookup(dfmToken, custom_dictionary), "data.frame")

# Column Bind original dataset with themes dataset
base = cbind(base, themes)

# run line 252 - 296 for 37-44
## Create overall ratio variables / PCA / PWT
base$environment_climate_ratio = round(base$environment_climate / base$words * 100, 2)
base$military_security_ratio = round(base$military_security / base$words * 100, 2)
base$indigenous_peoples_ratio = round(base$indigenous_peoples / base$words * 100, 2)
base$economy_resources_ratio = round(base$economy_resources / base$words * 100, 2)

groupes <- as.factor(base$caucus)
res.pca <- dudi.pca(base[,c(22:25)],
                    scannf = FALSE,
                    nf = 5)
pca_plot = fviz_pca_biplot(res.pca, repel = TRUE,
                           col.var = "black",
                           col.ind = groupes,
                           palette = c("cyan", "blue", "red", "orange"),
                           addEllipses = TRUE,
                           labelsize = 5,
                           pointsize = 4,
                           ellipse.type = "confidence",
                           label="var")
pca_plot +
  ggtitle("") +
  scale_shape_manual(values=c(15,16,17,8)) +
  theme(text = element_text(size = 15)) +
  theme(legend.title = element_blank()) +
  theme(legend.position = "bottom")

## pairwise t.test
# environment and climate
pwtt_environment_climate = pairwise.t.test(base$environment_climate_ratio,base$caucus,pool=F,p.adj="none")
round(pwtt_environment_climate$p.value, 2)
base %>% cohens_d(environment_climate_ratio ~ caucus)

# military and security
pwtt_military_security = pairwise.t.test(base$military_security_ratio,base$caucus,pool=F,p.adj="none")
round(pwtt_military_security$p.value, 2)
base %>% cohens_d(military_security_ratio ~ caucus)

# indigenous people
pwtt_indigenous_peoples = pairwise.t.test(base$indigenous_peoples_ratio,base$caucus,pool=F,p.adj="none")
round(pwtt_indigenous_peoples$p.value, 2)
base %>% cohens_d(indigenous_peoples_ratio ~ caucus)

# economy and resources
pwtt_economy_resources = pairwise.t.test(base$economy_resources_ratio,base$caucus,pool=F,p.adj="none")
round(pwtt_economy_resources$p.value, 2)
base %>% cohens_d(economy_resources_ratio ~ caucus)

# run line 302 to 422 to create the three subsets
### Create datasets for 37-38, 39-41, 42-44 legislation

# 37th to 38th (Lib. #1)
base3738 = subset(base, base$legislature <= 38)

# Create a length of themes variables
nb_Dictionary_Themes = length(custom_dictionary)

# List unique MPs
namelist = unique(base3738$nom)

# Create empty array
unique_MP_Array = array("template", dim=c(length(namelist),7))

# Loop for each unique MPs summary
for(i in 1:length(namelist)){
  temp = subset(base3738,base3738$nom == namelist[i])
  unique_MP_Array[i,1] = names(table(droplevels(temp)$nom))
  unique_MP_Array[i,2] = names(table(droplevels(temp)$caucus))
  unique_MP_Array[i,3] = sum(temp$words)
  unique_MP_Array[i,4] = sum(temp$environment_climate)
  unique_MP_Array[i,5] = sum(temp$military_security)
  unique_MP_Array[i,6] = sum(temp$indigenous_peoples)
  unique_MP_Array[i,7] = sum(temp$economy_resources)
}

# Create dataframe from array + change columns names
unique_MP_DB3738 = data.frame(unique_MP_Array)
colnames(unique_MP_DB3738)=c("nom","caucus", "words","environment_climate", "military_security", "indigenous_peoples", "economy_resources")

# Swap words + theme variables to numeric values instead of character values
unique_MP_DB3738$words = as.numeric(unique_MP_DB3738$words)
unique_MP_DB3738$environment_climate = as.numeric(unique_MP_DB3738$environment_climate)
unique_MP_DB3738$military_security = as.numeric(unique_MP_DB3738$military_security)
unique_MP_DB3738$indigenous_peoples = as.numeric(unique_MP_DB3738$indigenous_peoples)
unique_MP_DB3738$economy_resources = as.numeric(unique_MP_DB3738$economy_resources)

# Transform theme variables to reflect overall importance of each theme in their interventions
unique_MP_DB3738$environment_climate = round(unique_MP_DB3738$environment_climate / unique_MP_DB3738$words * 100, 2)
unique_MP_DB3738$military_security = round(unique_MP_DB3738$military_security / unique_MP_DB3738$words * 100, 2)
unique_MP_DB3738$indigenous_peoples = round(unique_MP_DB3738$indigenous_peoples / unique_MP_DB3738$words * 100, 2)
unique_MP_DB3738$economy_resources = round(unique_MP_DB3738$economy_resources / unique_MP_DB3738$words * 100, 2)

## 39th to 41st (Cons.)
base3941 = subset(base, base$legislature >= 39 & base$legislature <= 41)

# Create a length of themes variables
nb_Dictionary_Themes = length(custom_dictionary)

# List unique MPs
namelist = unique(base3941$nom)

# Create empty array
unique_MP_Array = array("template", dim=c(length(namelist),7))

# Loop for each unique MPs summary
for(i in 1:length(namelist)){
  temp = subset(base3941,base3941$nom == namelist[i])
  unique_MP_Array[i,1] = names(table(droplevels(temp)$nom))
  unique_MP_Array[i,2] = names(table(droplevels(temp)$caucus))
  unique_MP_Array[i,3] = sum(temp$words)
  unique_MP_Array[i,4] = sum(temp$environment_climate)
  unique_MP_Array[i,5] = sum(temp$military_security)
  unique_MP_Array[i,6] = sum(temp$indigenous_peoples)
  unique_MP_Array[i,7] = sum(temp$economy_resources)
}

# Create dataframe from array + change columns names
unique_MP_DB3941 = data.frame(unique_MP_Array)
colnames(unique_MP_DB3941)=c("nom","caucus", "words","environment_climate", "military_security", "indigenous_peoples", "economy_resources")

# Swap words + theme variables to numeric values instead of character values
unique_MP_DB3941$words = as.numeric(unique_MP_DB3941$words)
unique_MP_DB3941$environment_climate = as.numeric(unique_MP_DB3941$environment_climate)
unique_MP_DB3941$military_security = as.numeric(unique_MP_DB3941$military_security)
unique_MP_DB3941$indigenous_peoples = as.numeric(unique_MP_DB3941$indigenous_peoples)
unique_MP_DB3941$economy_resources = as.numeric(unique_MP_DB3941$economy_resources)

# Transform theme variables to reflect overall importance of each theme in their interventions
unique_MP_DB3941$environment_climate = round(unique_MP_DB3941$environment_climate / unique_MP_DB3941$words * 100, 2)
unique_MP_DB3941$military_security = round(unique_MP_DB3941$military_security / unique_MP_DB3941$words * 100, 2)
unique_MP_DB3941$indigenous_peoples = round(unique_MP_DB3941$indigenous_peoples / unique_MP_DB3941$words * 100, 2)
unique_MP_DB3941$economy_resources = round(unique_MP_DB3941$economy_resources / unique_MP_DB3941$words * 100, 2)

# 42nd to 44th (Lib. #2)
base4244 = subset(base, base$legislature >= 42 & base$legislature <= 44)

# Create a length of themes variables
nb_Dictionary_Themes = length(custom_dictionary)

# List unique MPs
namelist = unique(base4244$nom)

# Create empty array
unique_MP_Array = array("template", dim=c(length(namelist),7))

# Loop for each unique MPs summary
for(i in 1:length(namelist)){
  temp = subset(base4244,base4244$nom == namelist[i])
  unique_MP_Array[i,1] = names(table(droplevels(temp)$nom))
  unique_MP_Array[i,2] = names(table(droplevels(temp)$caucus))
  unique_MP_Array[i,3] = sum(temp$words)
  unique_MP_Array[i,4] = sum(temp$environment_climate)
  unique_MP_Array[i,5] = sum(temp$military_security)
  unique_MP_Array[i,6] = sum(temp$indigenous_peoples)
  unique_MP_Array[i,7] = sum(temp$economy_resources)
}

# Create dataframe from array + change columns names
unique_MP_DB4244 = data.frame(unique_MP_Array)
colnames(unique_MP_DB4244)=c("nom","caucus", "words","environment_climate", "military_security", "indigenous_peoples", "economy_resources")

# Swap words + theme variables to numeric values instead of character values
unique_MP_DB4244$words = as.numeric(unique_MP_DB4244$words)
unique_MP_DB4244$environment_climate = as.numeric(unique_MP_DB4244$environment_climate)
unique_MP_DB4244$military_security = as.numeric(unique_MP_DB4244$military_security)
unique_MP_DB4244$indigenous_peoples = as.numeric(unique_MP_DB4244$indigenous_peoples)
unique_MP_DB4244$economy_resources = as.numeric(unique_MP_DB4244$economy_resources)

# Transform theme variables to reflect overall importance of each theme in their interventions
unique_MP_DB4244$environment_climate = round(unique_MP_DB4244$environment_climate / unique_MP_DB4244$words * 100, 2)
unique_MP_DB4244$military_security = round(unique_MP_DB4244$military_security / unique_MP_DB4244$words * 100, 2)
unique_MP_DB4244$indigenous_peoples = round(unique_MP_DB4244$indigenous_peoples / unique_MP_DB4244$words * 100, 2)
unique_MP_DB4244$economy_resources = round(unique_MP_DB4244$economy_resources / unique_MP_DB4244$words * 100, 2)

### PCA on all three datasets (unique_MP_DB3738, unique_MP_DB3941, unique_MP_DB4244)

# run line 430 - 448 / PCA 37-38
## PCA 37-38
# Change rownames to MPs name instead of numbers
#rownames(unique_MP_DB3738) = unique_MP_DB3738$nom #this can be commented if you don't want the names on the PCA
groupes <- as.factor(unique_MP_DB3738$caucus)
res.pca <- dudi.pca(unique_MP_DB3738[,c(4:7)],
                    scannf = FALSE,
                    nf = 5)
pca_plot = fviz_pca_biplot(res.pca, repel = TRUE,
                           col.var = "black",
                           col.ind = groupes,
                           palette = c("cyan", "blue","red", "orange"),
                           addEllipses = TRUE,
                           labelsize = 5,
                           pointsize = 4,
                           ellipse.type = "confidence",
                           label="var") 
pca_plot +
  ggtitle("") +
  scale_shape_manual(values=c(15,16,17,8)) +
  theme(text = element_text(size = 15)) +
  theme(legend.title = element_blank()) +
  theme(legend.position = "bottom")

# run line 453 - 470 PWT / 37-38
## pairwise t.test
# environment and climate
pwtt_environment_climate = pairwise.t.test(unique_MP_DB3738$environment_climate,unique_MP_DB3738$caucus,pool=F,p.adj="none")
round(pwtt_environment_climate$p.value, 2)
unique_MP_DB3738 %>% cohens_d(environment_climate ~ caucus)

# military and security
pwtt_military_security = pairwise.t.test(unique_MP_DB3738$military_security,unique_MP_DB3738$caucus,pool=F,p.adj="none")
round(pwtt_military_security$p.value, 2)
unique_MP_DB3738 %>% cohens_d(military_security ~ caucus)

# indigenous people
pwtt_indigenous_peoples = pairwise.t.test(unique_MP_DB3738$indigenous_peoples,unique_MP_DB3738$caucus,pool=F,p.adj="none")
round(pwtt_indigenous_peoples$p.value, 2)
unique_MP_DB3738 %>% cohens_d(indigenous_peoples ~ caucus)

# economy and resources
pwtt_economy_resources = pairwise.t.test(unique_MP_DB3738$economy_resources,unique_MP_DB3738$caucus,pool=F,p.adj="none")
round(pwtt_economy_resources$p.value, 2)
unique_MP_DB3738 %>% cohens_d(economy_resources ~ caucus)

## Run line 474 - 493 / PCA 39-41
#rownames(unique_MP_DB3941) = unique_MP_DB3941$nom #this can be commented if you don't want the names on the PCA
groupes <- as.factor(unique_MP_DB3941$caucus)
res.pca <- dudi.pca(unique_MP_DB3941[,c(4:7)],
                    scannf = FALSE,
                    nf = 5)

pca_plot = fviz_pca_biplot(res.pca, repel = TRUE,
                           col.var = "black",
                           col.ind = groupes,
                           palette = c("cyan", "blue","red", "orange"),
                           addEllipses = TRUE,
                           labelsize = 5,
                           pointsize = 4,
                           ellipse.type = "confidence",
                           label="var")
pca_plot +
  ggtitle("") +
  scale_shape_manual(values=c(15,16,17,8)) +
  theme(text = element_text(size = 15)) +
  theme(legend.title = element_blank()) +
  theme(legend.position = "bottom")

## Run line 497 - 514 pairwise t.test 39-41
# environment and climate
pwtt_environment_climate = pairwise.t.test(unique_MP_DB3941$environment_climate,unique_MP_DB3941$caucus,pool=F,p.adj="none")
round(pwtt_environment_climate$p.value, 2)
unique_MP_DB3941 %>% cohens_d(environment_climate ~ caucus)

# military and security
pwtt_military_security = pairwise.t.test(unique_MP_DB3941$military_security,unique_MP_DB3941$caucus,pool=F,p.adj="none")
round(pwtt_military_security$p.value, 2)
unique_MP_DB3941 %>% cohens_d(military_security ~ caucus)

# indigenous people
pwtt_indigenous_peoples = pairwise.t.test(unique_MP_DB3941$indigenous_peoples,unique_MP_DB3941$caucus,pool=F,p.adj="none")
round(pwtt_indigenous_peoples$p.value, 2)
unique_MP_DB3941 %>% cohens_d(indigenous_peoples ~ caucus)

# economy and resources
pwtt_economy_resources = pairwise.t.test(unique_MP_DB3941$economy_resources,unique_MP_DB3941$caucus,pool=F,p.adj="none")
round(pwtt_economy_resources$p.value, 2)
unique_MP_DB3941 %>% cohens_d(economy_resources ~ caucus)

## run line 518 - 536 / PCA 42-44
#rownames(unique_MP_DB4244) = unique_MP_DB3941$nom #this can be commented if you don't want the names on the PCA
groupes <- as.factor(unique_MP_DB4244$caucus)
res.pca <- dudi.pca(unique_MP_DB4244[,c(4:7)],
                    scannf = FALSE,
                    nf = 5)
pca_plot = fviz_pca_biplot(res.pca, repel = TRUE,
                           col.var = "black",
                           col.ind = groupes,
                           palette = c("blue","red", "orange"), # Remove "cyan", there is no BQ MP interventions
                           addEllipses = TRUE,
                           labelsize = 5,
                           pointsize = 4,
                           ellipse.type = "confidence",
                           label="var")
pca_plot +
  ggtitle("") +
  scale_shape_manual(values=c(15,16,17,8)) +
  theme(text = element_text(size = 15)) +
  theme(legend.title = element_blank()) +
  theme(legend.position = "bottom")

## run line 540 - 557 / pairwise t.test 42-44
# environment and climate
pwtt_environment_climate = pairwise.t.test(unique_MP_DB4244$environment_climate,unique_MP_DB4244$caucus,pool=F,p.adj="none")
round(pwtt_environment_climate$p.value, 2)
unique_MP_DB4244 %>% cohens_d(environment_climate ~ caucus)

# military and security
pwtt_military_security = pairwise.t.test(unique_MP_DB4244$military_security,unique_MP_DB4244$caucus,pool=F,p.adj="none")
round(pwtt_military_security$p.value, 2)
unique_MP_DB4244 %>% cohens_d(military_security ~ caucus)

# indigenous people
pwtt_indigenous_peoples = pairwise.t.test(unique_MP_DB4244$indigenous_peoples,unique_MP_DB4244$caucus,pool=F,p.adj="none")
round(pwtt_indigenous_peoples$p.value, 2)
unique_MP_DB4244 %>% cohens_d(indigenous_peoples ~ caucus)

# economy and resources
pwtt_economy_resources = pairwise.t.test(unique_MP_DB4244$economy_resources,unique_MP_DB4244$caucus,pool=F,p.adj="none")
round(pwtt_economy_resources$p.value, 2)
unique_MP_DB4244 %>% cohens_d(economy_resources ~ caucus)