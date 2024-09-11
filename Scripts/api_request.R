
# api request -------------------------------------------------------------
#####################################################################################
#Denne skripten gjør først en API request til SSB for å få befolkning informasjon   #
#om Karmøy etter alder, kjønn og grunnkrets. Etterkant samler grunnkrets til        #
#post kode nivå før det beregner gruppe andel. Informasjon kan brukes for å beregne #
#utvalge størrelse for forsjellige spørreundersøkelser.                             #
#Om det er behov for endringer i datasett fra API, kan det gjøres gjennom endringer #
#i api querry. Også se                                                              #
#https://www.ssb.no/en/api/api-eksempler-pa-kode                                    #
#for å få oversikt på bruk av SSB API                                               #
#####################################################################################



# setup -------------------------------------------------------------------

if (isFALSE(require(pacman))) install.packages("pacman") else library(pacman)

p_load(char = c("rjstat","httr","klassR","tidyverse"))



# Get dataset -------------------------------------------------------------

ssb.url<- "https://data.ssb.no/api/v0/no/table/" #det er generelt SSB API URL
tabell.nr<- "04362" #det er tabell nr. for tabellen som skal brukes

api.call<- paste0(ssb.url,tabell.nr)
# om en annen tabell ønskes eller må endre en parameter kan det gjøres her som endring på api, querry
# FE, om det er ønskelig å få data fra tidligere, legg årene til codes:Tid, value: [2024,...]

api.querry<- '{
  "query": [
    {
      "code": "Region",
      "selection": {
        "filter": "agg_single:GrkretsNy",
        "values": [
          "11490101",
          "11490102",
          "11490103",
          "11490104",
          "11490105",
          "11490201",
          "11490202",
          "11490203",
          "11490204",
          "11490301",
          "11490302",
          "11490303",
          "11490304",
          "11490305",
          "11490306",
          "11490307",
          "11490308",
          "11490309",
          "11490401",
          "11490402",
          "11490403",
          "11490404",
          "11490501",
          "11490502",
          "11490503",
          "11490504",
          "11490505",
          "11490506",
          "11490507",
          "11490508",
          "11490509",
          "11490510",
          "11490511",
          "11490512",
          "11490513",
          "11490601",
          "11490602",
          "11490603",
          "11490604",
          "11490605",
          "11490606",
          "11490607",
          "11490608",
          "11490609",
          "11490610",
          "11490701",
          "11490702",
          "11490703",
          "11490704",
          "11490705",
          "11490706",
          "11490801",
          "11490802",
          "11490803",
          "11490804",
          "11490805",
          "11490806",
          "11490901",
          "11490902",
          "11490903",
          "11490904",
          "11490905",
          "11490906",
          "11490907",
          "11490908",
          "11490909",
          "11491001",
          "11491002",
          "11491003",
          "11491004",
          "11491007",
          "11491008",
          "11491009",
          "11491010",
          "11491011",
          "11491012",
          "11491013",
          "11491014",
          "11491015",
          "11499999"
        ]
      }
    },
    {
      "code": "Kjonn",
      "selection": {
        "filter": "item",
        "values": [
          "1",
          "2"
        ]
      }
    },
    {
      "code": "Alder",
      "selection": {
        "filter": "item",
        "values": [
          "00-05",
          "06-15",
          "16-19",
          "20-24",
          "25-29",
          "30-49",
          "50-59",
          "60-66",
          "67-69",
          "70-79",
          "080+"
        ]
      }
    },
    {
      "code": "Tid",
      "selection": {
        "filter": "item",
        "values": [
          "2024"
        ]
      }
    }
  ],
  "response": {
    "format": "json-stat2"
  }
}'

api.response<- POST(url = api.call,
                    body = api.querry,
                    encode = "json") %>% #siden vi senden querry som en string, må vi også legge til encode argument med "json"
  content(.,type = "text") #vi utvinner body (innholdet data) fra api respons som txt so vi kan parse det til datasett

api.data<- fromJSONstat(x = api.response,
                        naming = "label",
                        use_factors = F)

shares<- api.data %>% mutate(perc = value/sum(value))
# grunnkrets koder --------------------------------------------------------

grunnkrets_nummer<- klassR::GetKlass(klass = 1) %>% 
  filter(grepl(x = code,"^1149",perl = T)) %>% 
  select(code,name)


api.data<-left_join(api.data,grunnkrets_nummer, by = c("region" = "name"))
