# I want to analyze this Taumata Arowai data to see if there's any trends in this consumer advisory water data. 
  # Taumata Arowai is the water watchdog for New Zealand. 

# Long-term consumer advisories "likely indicated systemic problems with the supply," according to a 2022 Taumata Arowai report.
 # https://www.taumataarowai.govt.nz/assets/Uploads/Governance-docs/Drinking-Water-Regulation-Report-2022.pdf

# As of 04/04/24, Taumata Arowai said I had found an incorrect repeat, so I am running this again to check there aren't any changes after getting updated data.
  # Per an email from Leanna Bruce with Taumata Arowai:
    # "The Tuapeka West entry was a duplicate in our system. We have now removed it and attached an updated list for you. There don’t appear to be any other duplicates."

# I found even more repeats. Taumata Arowai sent data on 04/18/24 that allegedly removed repeats. 
  # The repeats came from long-term consumer advisories that started and ended around the same time. 

# Taumata Arowai admitted they had incorrect repeats and then resent data with the repeats removed as of April 19. 

install.packages("tidyverse")
library(tidyverse)

install.packages("janitor")
library(janitor)

library(readr)
consumer_advisory_21_23_3 <- read_csv("consumer_advisory_21_23_3.csv")
View(consumer_advisory_21_23_3)

# 4th iteration of this, leggo:

library(readr)
consumer_advisory_21_23_4_6 <- read_csv("Data/consumer_advisory_21_23_4.6.csv")
View(consumer_advisory_21_23_4_6)



# Using a Mac, important to remember that Cmd + Shift + M is the shortcut to get the '%>%' symbol

###### Cleaning up the data to analyze ######
  # I had used a formula to pull out just the year from when the long-term advisory occured. 
  # Need the column names to be consistent. Will use janitor package for that. 

consumer_advisory_21_23 <- consumer_advisory_21_23_4_6

consumer_advisory_21_23 %>% 
  clean_names() %>% 
  View()
  
  # Worked swimmingly

  advisory <- consumer_advisory_21_23 %>% 
    clean_names()
  
  # Going to export so I can do this simultaneously in SQL
  
  advisory %>% write_csv("advisory.csv", na = "")

##### Seeing where this happens most often as of March 19, 2024. 
  
  advisory %>% 
    group_by(supplier_name, supply_name) %>% 
    summarize(count = n()) %>% 
    View()
  
    # Weird blanks tucked in the advisory data for some reason. 
  
  advisory %>% 
    filter(!is.na(supplier_name)) %>%
    View()
  
      # Cool, cool - 116 rows like it should be 
  
        advisory <- advisory %>% 
          filter(!is.na(supplier_name))
        
        advisory %>% write_csv("advisory.csv", na = "")
        
  
    # Worked, but there are some repeats in there that I want to remove.  
  
  advisory %>% 
    distinct(consumer_advisory_start_date, .keep_all = TRUE)
   
  unique_advisory <- advisory %>% 
    distinct(consumer_advisory_start_date, .keep_all = TRUE)
  
    # Cut it down to quite a few rows, may have been too broad of a filter
  
        advisory %>% 
          distinct(supplier_name, supply name, consumer_advisory_start_date, .keep_all = TRUE)
        
        # Didn't work - going to try to do it by additional grouping principles
        
        advisory %>% 
          group_by(supplier_name, supply_name, consumer_advisory_start_date) %>% 
          summarize(count = n()) %>%
          View()

        # Did good! 
          # No longer any repeats. Previously, Tuapeka West orders within the Clutha District Council is the only advisory at the same start date with the same supply and supplier. 
          # Similar results in SQL:
            # select supplier_name, supply_name, consumer_advisory_start_date, count(*)
            # from advisory
            # group by supplier_name, supply_name, consumer_advisory_start_date
        
#### When needed to identify rogue repeats like Tuapeka West. That has been identified. This is what was used to clean and identify issues: #########        
        # Try another option:
        
        advisory %>% 
          select(supplier_name, supply_name, consumer_advisory_start_date, consumer_advisory_end_date, supply_population) %>% 
          group_by(supplier_name, supply_name, consumer_advisory_start_date) %>% 
          summarize(count = n()) %>%
          View()
        
        # Works now, didn't work before. I think it's because there are slightly different end dates for the Tuapeka West long-term advisories and thus adding that row back to it.
         
        # Try to add population: 
          
          advisory %>% 
            group_by(supplier_name, supply_name, consumer_advisory_start_date, supply_population) %>% 
            summarize(count = n()) %>%
            View()
          
          # Worked great!
          
          unique_advisory <-  advisory %>% 
            group_by(supplier_name, supply_name, consumer_advisory_start_date, supply_population) %>% 
            summarize(count = n()) 
          
          # Count how many suppliers had repeat long-term consumer advisories. 
  
              unique_advisory %>% 
              group_by(supplier_name, supply_name, supply_population) %>% 
              summarize(count = n()) %>% 
              arrange(desc(count))  %>% 
              View()
        
              
#### How many repeats? #########  
              
              advisory %>% 
                group_by(supplier_name, supply_name, supply_population) %>% 
                summarize(count = n()) %>% 
                arrange(desc(count)) %>% 
                View()
              
      # There are now 3 councils/supply areas with repeat long-term consumer advisories. Previously, it was 11. 

########################## REPEATS IN DATA DEBACLE AND TROUBLESHOOTING ########### 
              
           # I went back to Taumata Arowai asking about why this was different. They said in a phone call that it was a reflection of a system that does not properly capture when an order ended or began.   
            # There were other long-term consumer advisories that have different, not even similar, start and end dates in versions 1 and 2 of the data that is no longer in version 3 or 4. 
              # From Leanna Bruce: 
                # "In our system the existing consumer advisory records were deactivated which should not have occurred – any duplicate records that should only relate to a single event needed to be deleted from the database and system permissions to do this are limited.  These duplicate records were a result of human error.  We are now rechecking all our records and deleting duplicates where appropriate, so this reporting error does not reoccur."
              # In summary -- some of these long-term advisories go on for so long that Taumata Arowai didn't realize an order was already in place when they went to put a new one in. 
               
      # Previous data findings were:        
        # Hundreds of people utilize the water in most of the instances -- as much as 4,550 from Downlands in the Timaru District to as low as 29 people.
        # Clutha District Council has two spots with repeats:
              # Clutha District Council           Glenkenich Rural                   705     2
              # Clutha District Council           Tuapeka West                       276     2
              
              # Richardson Rural long-term advisory ended recently, in January 2024, after lasting nearly a year. 
              
       ##  PREVIOUS AND NOT CURRENT:   ##  
             # supplier_name                     supply_name          supply_population count
             # 1 Ashburton District Council        Montalto                            90     2
             # 2 Clutha District Council           Glenkenich Rural                   705     2
             #  3 Clutha District Council           Tuapeka West                       276     2
             #  4 Hautope Water Scheme Incorporated Hautope Water Scheme                50     2
             # 5 Hurunui District Council          Balmoral Rural                     273     2
             # 6 Kapiti Coast District Council     Hautere                            930     2
             # 7 Selwyn District Council           Springfield                        616     2
             #  8 Timaru District Council           Downlands                         4550     2
             # 9 Timaru District Council           St Andrews                         280     2
             # 10 Waitaki District Council          Bushy Creek                         29     2
             # 11 Waitaki District Council	        Stoneburn	                        86	    2
              
       # SQL showed the same results initially (11 repeats), but now shows three repeats. 
          #    select supplier_name, supply_name, supply_population, count(*)
          #    from advisory
          #    group by supplier_name, supply_name, supply_population
          #    order by count(*) DESC    
    
      #  MAY BE REPEATS BECAUSE OF SIMILAR START DATES IN WAITAKI AND SELWYN DISTRICT COUNCILS. REQUEST FOR CLARIFICATION AS OF APRIL 16 AT 8 AM       
        # Most of these were errant repeats from Taumata Arowai. See above.           
              
######### Pulling out just the repeats: #########
              
              advisory %>% 
                group_by(supplier_name, supply_name) %>% 
                summarize(count = n()) %>% 
                filter(count >= 2)
              
              # Filter worked. Let's see if we can pull out the start, stop and length.
              
              # supplier_name                     supply_name          count
              # Clutha District Council           Glenkenich Rural         2
              # Hautope Water Scheme Incorporated Hautope Water Scheme     2
              # Waitaki District Council          Stoneburn                2
              
              
              repeat_lta <- advisory %>% 
                group_by(supplier_name, supply_name) %>% 
                summarize(count = n()) %>% 
                filter(count >= 2)
              
              repeat_lta %>% inner_join(advisory) %>% 
                View()
              
                # And the inner join worked too!
              
                  detailed_repeat_lta <- repeat_lta %>% inner_join(advisory)
                      
            # Now the median and average:
          
                  library(dplyr)
                  
                  detailed_repeat_lta %>% 
                    summarize(mean = mean(consumer_advisory_age_in_days),
                              median = median (consumer_advisory_age_in_days)) %>% 
                    View()
                  
                # In Clutha, median/average long-term consumer advisory lasted 203 days. But - that's because one lasted 350 days and another for 50ish days.
                  
                  # supplier_name                      mean median
                  # Clutha District Council             203    203
                  # Hautope Water Scheme Incorporated    35     35
                  # Waitaki District Council            145    145
                  
                  detailed_repeat_lta %>%
                    summarise_at(c("consumer_advisory_age_in_days"), mean, na.rm = FALSE)
                  
                    # Same median as above. Not super relevant bc two two advisories but good to know. 
                  
            # For the median per supplier:
                  
                  detailed_repeat_lta %>% 
                    summarize(median = median(consumer_advisory_age_in_days))
                  
                  # Is now working after a restart and updated packages. Was giving me problems before. 
                    # Getting it down to just one column is not working despite this being the proven code that works, so going to export to Excel:
                    # See the code on the Hispanic/Latino teacher story: https://github.com/ZackNewsMan/9WTK_co_hispanic_latino_teachers/blob/main/hispanic_latino_teach_student.R  
                  
                    detailed_repeat_lta %>% write_csv("detailed_repeat_lta.csv", na = "")
              
                    
                    
  # Suppliers with repeated long-term advisories in multiples supplies       
       
   detailed_repeat_lta %>% 
     group_by(supplier_name) %>% 
     summarize(count = n()) %>% 
     arrange(desc(count))
   
    # Three suppliers have two long-term consumer advisories in their purview
       # supplier_name                     count
       # Clutha District Council               2
       # Hautope Water Scheme Incorporated     2
       # Waitaki District Council              2
       
    # Closer look at Clutha:
       
       detailed_repeat_lta %>% 
         filter(supplier_name == "Clutha District Council")  %>% 
         View() 
       
       # Two at Glenkenich Rural (what we know about from repeat above)
       
       
       # Closer look at Hautope:
       
       detailed_repeat_lta %>% 
         filter(supplier_name == "Hautope Water Scheme Incorporated")  %>% 
         View() 
       
       # All are what we know about from repeat above. Nothing new came from this.   
                    
##### Suppliers with most long-term advisories: #########       
          
              advisory %>% 
                group_by(supplier_name) %>% 
                summarize(count = n()) %>% 
                arrange(desc(count))  %>% 
                View()          
  
        # Department of Conservation has the most, but most of these appear to be campsites:
            # Top 10:
             # Department of Conservation            35
             # Ministry of Education                 10
             # Waitaki District Council               9
             # Clutha District Council                7
             # Buller District Council                4
             # Timaru District Council                4
             # Waimate District Council               4
             # Hurunui District Council               3
             # Otorohanga District Council            3
             # Thames Coromandel District Council     3

            # Findings echoed in SQL:
              # select supplier_name, count(supplier_name), count(*)
              # from advisory
              # group by supplier_name
              # order by count(supplier_name) DESC
                            
        # This is interesting because there are boil order signs that blanket many of these campsites but in multiple personal instances DOC rangers claim they drink the water without issue.
              
              
#### Largest consumer advisory? #######

  advisory %>% 
    group_by(supplier_name, supply_population) %>% 
    summarize(lta_count = n()) %>% 
    arrange(desc(supply_population))
    
    # Long-term advisory for the largest population was the September 2023 boil water order for Queenstown.
      # Population: 44,708

   # Verified in SQL:
     # SELECT supplier_name, supply_population
     #  from advisory
     # group by supplier_name
     # ORDER by supply_population DESC
   
  # Total population affected by a long-term consumer advisory
       
       advisory %>% 
         summarize(population_total = sum(supply_population))
       
       # 90,557
       
        # Verified in SQL, 90,557
           # select sum(supply_population)
           # from advisory
                                
###### Date difference check   #######
     
library(lubridate)              

advisory %>% 
difftime(consumer_advisory_end_date, consumer_advisory_start_date, units = 'days')
 
  # Hmm. Getting a POSIX error that way

# Using this tutorial: https://www.youtube.com/watch?v=KBErorVelwk 
             
advisory$new_start <- mdy(advisory$consumer_advisory_start_date)              
  
  # Looks like that worked great!

advisory$new_end <- mdy(advisory$consumer_advisory_end_date)              

  # That worked too

difftime(advisory$new_end, advisory$new_start, units = 'days') %>% 
  View()

# gonna try in dplyr

advisory %>% 
  mutate(start_date = mdy(consumer_advisory_start_date)) %>% 
  mutate(end_date = mdy(consumer_advisory_end_date)) %>% 
  View()

  # That worked too, definitely easier to format this way. 

  difftime(advisory$new_end - advisory$new_start)

  # that didn't work. not sure what to do 
  
    # Pulled out the "advisory" CSV into an Excel to use the datedif formula. Pretty much aligned
      # =DATEDIF(G2,H2,"d")
        # https://support.microsoft.com/en-us/office/calculate-the-difference-between-two-dates-8235e7c9-b430-44ca-9425-46100a162f38 
      # Only variation happened when there wasn't a clear end date. Then Excel added one more date than Tauma Arowai had calculated. 
      # So I think it is safe as long as use "about" and some approximate terminology.

#### Longest consumer advisory? ####### 
  
  advisory %>% 
    select(supplier_name, supply_name, supply_population, consumer_advisory_start_date, consumer_advisory_end_date, consumer_advisory_age_in_days) %>% 
    group_by(supplier_name, supply_name, supply_population) %>% 
    arrange(desc(consumer_advisory_age_in_days)) %>% 
    View()
  
  # Top 10 longest long-term advisories happened for years, longest since 1992 and 1994. 
    # Waitaki District Council had three of the longest 10. 
    # 51 happen for a year or longer as of 19 March 2024
  
      # supplier_name                   supply_name          supply_population consumer_advisory_st…¹ consumer_advisory_en…² consumer_advisory_ag…³
  # Matamata Piako District Council        Te Aroha                 3838                9/30/92                NA                                      11492
  # Tukurua Water Supply Society Incorpor… Tukurua                   100                 11/7/94                NA                                      10724
  # Tarawera Co-Operative Water Supply     Tarawera C…                50                 1/4/14                 NA                                       3726
  # Auckland Council                       Whatipu, H…                50                3/13/14                NA                                       3658
  # Waitaki District Council               Bushy Creek                29                 1/21/15                NA                                       3344
  # Waitaki District Council               Ohau Alpin…                36                  5/11/15                NA                                       3234
  # Hurunui District Council               Hurunui #1                681                 1/1/16                 NA                                       2999
  # Wairoa District Council                Mahanga Be…                50                8/22/16                NA                                       2765
  # Masterton District Council             Wainuioru …               184                11/21/16               NA                                       2674
  # Hurunui District Council               Kaiwara                   129                 1/1/17                 NA                                       2633
  
    # Echoed in SQL:
      # SELECT supplier_name, supply_name, supply_population, consumer_advisory_age_in_days
      # FROM advisory
      # order by consumer_advisory_age_in_days DESC
  
  # Filter out for just 365 or more:
  
  advisory %>%   
    select(supplier_name, supply_name, supply_population, consumer_advisory_start_date, consumer_advisory_end_date, consumer_advisory_age_in_days) %>% 
    group_by(supplier_name, supply_name, supply_population) %>% 
    filter(consumer_advisory_age_in_days >= 365) %>% 
    arrange(desc(consumer_advisory_age_in_days)) %>% 
    View()
  
    # SQL verified, last a year or more in 51 instances.
      # SELECT supplier_name, supply_name, supply_population, consumer_advisory_age_in_days
      # FROM advisory
      # where consumer_advisory_age_in_days >= 365
      # order by consumer_advisory_age_in_days DESC
  
  advisory_365 <- advisory %>%   
    select(supplier_name, supply_name, supply_population, consumer_advisory_start_date, consumer_advisory_end_date, consumer_advisory_age_in_days) %>% 
    group_by(supplier_name, supply_name, supply_population) %>% 
    filter(consumer_advisory_age_in_days >= 365) %>% 
    arrange(desc(consumer_advisory_age_in_days))
  
  # Which supplier has the most year-plus advisories:
  
  advisory_365 %>% 
    select(supplier_name, supply_name, supply_population, consumer_advisory_start_date, consumer_advisory_end_date, consumer_advisory_age_in_days) %>% 
    group_by(supplier_name) %>% 
    summarize(count = n()) %>% 
    arrange(desc(count))  %>% 
    View()          
  
    # The Buller District Council has the most with four year-plus long-term advisories
  
        # Top 12 (multiple have two)
          # supplier_name         count
          # Buller District Council	4
          # Waimate District Council	3
          # Timaru District Council	3
          # Thames Coromandel District Council	3
          # Otorohanga District Council	3
          # Department of Conservation	3
          # Clutha District Council	3
          # Waitaki District Council	2
          # Wairoa District Council	2
          # Ministry of Education	2
          # Matamata Piako District Council	2
          # Hurunui District Council	2
  
      # Echo'd in SQL:
          # CREATE TABLE "advisory_365" as
          # SELECT supplier_name, supply_name, supply_population, consumer_advisory_age_in_days
          # FROM advisory
          # where consumer_advisory_age_in_days >= 365
          # order by consumer_advisory_age_in_days DESC
          
          # select supplier_name, count(supplier_name)
          # from advisory_365
          # group by supplier_name
          # order by  count(supplier_name) DESC
  
  
  # Will group and export t omake the graphic
  
  
  grouped_advisory_365 <- advisory_365 %>% 
    select(supplier_name, supply_name, supply_population, consumer_advisory_start_date, consumer_advisory_end_date, consumer_advisory_age_in_days) %>% 
    group_by(supplier_name) %>% 
    summarize(count = n()) %>% 
    arrange(desc(count))
  
  grouped_advisory_365 %>% write_csv("grouped_advisory_365.csv", na = "")
  

############### types of long-term advisories in data ##########
  
  # what types of advisories are in the data?
    # types: https://www.taumataarowai.govt.nz/for-communities/emergencies/drinking-water-notices/ 
  
  advisory %>% 
    group_by(consumer_advisory_category) %>% 
    summarize(count = n()) 
  
  #  consumer_advisory_category count
  # Boil Water                   108
  # Do Not Drink                   8
  
  
########### Ministry of Education advusories ##########
  
  
  advisory %>% 
    filter(supplier_name == "Ministry of Education") %>% 
    View()
  
  education_ltca <- advisory %>% 
    filter(supplier_name == "Ministry of Education")
  
  education_ltca %>% write_csv("education_ltca.csv", na = "")
  
############# Any repeats among short-term advisories? ###########
  
  library(readr)
  short_advisories <- read_csv("short-term advisories/short_advisories.csv")
  View(short_advisories)
    
  short <- short_advisories
  
# standardize name format  
               
  short <- short %>% 
    clean_names()
  
  
  # let's sort by supply name. No supplier name in data
  
  short %>% 
    group_by(supply) %>% 
    summarize(count = n()) %>% 
    View()
  
# Weird blanks tucked in the advisory data again because it is redacted and other reasons  
  
  short %>% 
    filter(!is.na(supply)) %>% 
    View()
  
  # Worked well
  
  short_filtered <- short %>% 
    filter(!is.na(supply))
  
  # Again:
  
  short_filtered %>% 
    group_by(supply) %>% 
    summarize(count = n()) %>% 
    arrange(desc(count))  %>% 
    View()
  
  # Just to two or more:
  
  short_filtered %>% 
    group_by(supply) %>% 
    summarize(count = n()) %>% 
    filter(count >= 2) %>% 
    arrange(desc(count))
  
  # Lots of redacted supplies affected and 42 repeats with at least 2 times a supply was affected. 
    # Unsure if it is the same supplier and supply though. 
  
  repeat_short_filtered <- short_filtered %>% 
    group_by(supply) %>% 
    summarize(count = n()) %>% 
    filter(count >= 2) %>% 
    arrange(desc(count))
  
  
# Let's put the repeats back into the data before doing a broader join. 
  
  repeat_short_filtered %>% inner_join(short_filtered) %>% 
    View()
  
  # And the inner join worked again too!
  
  detailed_repeat_short_filtered <- repeat_short_filtered %>% inner_join(short_filtered) 
  
########## JOIN SHORT AND LONG TERM ###########
 # I changed the supply_name to supply in the long-term advisory data just so I could do an inner join on that column
  
  library(readr)
  consumer_advisory_21_23_4_5 <- read_csv("consumer_advisory_21_23_4.5.csv")
  View(consumer_advisory_21_23_4_5)
  
  
  consumer_advisory_21_23_4_5 <- consumer_advisory_21_23_4_5 %>% 
    clean_names()
  
  consumer_advisory_21_23_4_5 <- consumer_advisory_21_23_4_5 %>% 
    filter(!is.na(supplier_name))
  
  short_filtered %>% inner_join(consumer_advisory_21_23_4_5) %>% 
    View()
   
  # 34 lines where a short-term advisory happened to the same supply as a long-term, but idk how Earth-shattering that is. 
    # I would assume that you would have a short-term where a long-term is. 
    # Would need to confirm in SQL 
  
############# check how many council supplies have a protozoa barrier ##############

  # Downloaded from: https://www.taumataarowai.govt.nz/about/water-services-insights-and-performance/	
  #  Excel sheet from this section on Aug 5 2024: Drinking Water Regulation Report 2023   
    
  library(tidyverse)
  library(janitor)
  
  library(readr)
  water_barrier_ta_2023 <- read_csv("water_barrier_ta_2023.csv")
  View(water_barrier_ta_2023)
  
  barrier <- water_barrier_ta_2023 %>% 
    clean_names()
  
  # Export to follow along in SQL
  
  barrier %>% write_csv("barrier.csv", na = "")
  

# Count council vs non-council supplies 
  
  barrier %>% 
    group_by(is_council) %>% 
    summarize(count = n()) 
  
  # Department of Conservation and Ministry of Education are in the "not council" designation as of now. May need to change that
  
  # is_council count
  # No           982
  # Yes          486
  
  # Same thing in SQL. 486 council supplies in the data. 
  
    # SELECT is_council, count(is_council)
    # from barrier
    # GROUP by is_council
    
# Pull out just the council supplies 
    
  council <- barrier %>% 
    filter(is_council == "Yes")
  
# How many council supplies have a protozoa barrier and for what total population?  
  
  # any repeats?
  
  council %>% 
    group_by(supply_id) %>% 
    summarize(count = n()) %>% 
    arrange(desc(count))
  
    # NO REPEATS
  
  # double check
  
      council %>% 
        group_by(supply_id) %>% 
        summarize(count = n()) %>% 
        View()
  
    # nope, no repeats
  
  council %>% 
    group_by(protozoa_barrier) %>% 
    summarize(count = n(),
              supply_population = sum(supply_population)) 
  
  
  # 94 definitely did not have a protozoa barrier:
  
  # protozoa_barrier count population
  #  No                  94     737065
  # Not Applicable      44         NA
  # Not determined       1        293
  # Yes                347    1616414
  
  
  # Findings echo'd in SQL:
  
    # create table "council" as
    # SELECT *
    #  FROM barrier
    # WHERE is_council = "Yes"
    
    # SELECT protozoa_barrier, count(protozoa_barrier), sum(supply_population)
    # from council
    # GROUP by protozoa_barrier
  
  # Double checked the yes with this SQL query:
  
      # SELECT *
      #   FROM council
      # WHERE protozoa_barrier = "Yes"
  
  
 94+347  
 
 # total number of council supplies that definitely either do/don't have protozoa: 441
  
94/441  
  
  # That means 21%%, or 1 in 5, council supplies don't have a protozoa barrier. 
    # 737,065 served by a supply without protozoa barrier. 
      # Taumata Arowai estimates the number of those actually affected by a supply without a protozoa barrier is less - 489,576.

      # The explanation for this discrepancy from LeeAnn James, spokesperson from Taumata Arowai, is a possible difference in the way the columns are calculated. 
        # The "supply population" and "distribution zone population" should be the same but is not.
    # From LeeAnn James: 
        # "The first population figure that drinking water suppliers provide in Hinekōrako is ‘supply population’. This is the total population supplied drinking water by a supply.
        # "The second is ‘distribution zone population’, which is the population in a specific area serviced by a reticulated supply.
        # "The population of all of the distribution zones that make up a supply should add up to the total supply population, although there are sometimes variances in the data that we encourage suppliers to identify and correct."


# for bacteria barrier

council %>% 
  group_by(bacteria_barrier) %>% 
  summarize(count = n(),
            supply_population = sum(supply_population)) 

    # bacteria_barrier count 
    # No                  20             
    # Not Applicable      28                
    # Not determined       1               
    # Yes                437  

  # Echo'd in SQL:

    # SELECT bacteria_barrier, count(bacteria_barrier)
    # from council
    # GROUP by bacteria_barrier

# for residual disinfection

council %>% 
  group_by(residual_disinfection) %>% 
  summarize(count = n()) 

  # residual_disinfection  count
  # No                       33
  # Not Applicable           74
  # Not determined            1
  # Yes                     378
  
  # Samesies in SQL:
  
  # SELECT residual_disinfection, count(residual_disinfection)
  # from council
  # GROUP by residual_disinfection
    
  
  
  