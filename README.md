# shiny_beedata

## Introduction 

The data set that I am using represents the individual bee responses of different concentrations of the insecticide imidacloprid, under controlled laboratory conditions.  This data was obtained from research performed in South Mississippi, Pearl River County.   

The raw beetox data set introduces you to 12 different bee species providing information on the Level of Sociality, Degree of Floral Specialization, Floral Host, Bee Genus, the Date Captured, Bee Sex, Imidacloprid concentration (parts per billion), the Bee Longevity in Bioassay (days), Days Paralyzed and Days Active.  

The concentration of the insecticide ranges from 0 - 100 parts per billion
```{r}
# Calculate the range of the concentration used on the bees
concentration_range <- range(beetox_data$`Imidacloprid concentration (parts per billion)`)
concentration_range
```


### About the data

*“Level of Sociality”* can either be social or solitary.  First, a solitary bee is one that lives alone and does not swarm.  This type of bee rarely stings, collect pollen and nectar from different flowers for their eggs and dig tunnels in the ground or use wood with holes.  These bees are said the be “more efficient” in pollinating plants.  Next, a social bee is known for making honey within their hive.  These bee will sting if they feel threatened, they will have a Queen bee that is the largest bee in the hive, the females will be the workers and the males will be the drone bees.

*“Degree of Floral Specialization”* will either be polylectic or oligolectic.  First, a polylectic bee species will gather pollen from multiple genera in more than one plant family, whereas a oligolectic bee species will gather pollen from two to several species in one plant family.  

*“Floral Host”* describes what type of flower the bee uses.  This will vary between Cucurbita, Hive, Okra, Sunflower and Vaccinum.  Some species will use more than one type of plant.

With this data I want people to explore the insecticidal impacts that different concentrations of imidacloprid have on bees in laboratory conditions.  

### Questions to Consider

**What Bees are Most Affected by the Insecticide Imidocloprid?** 

Are insecticides harming our pollinators?  Is there a certain gender that experiences greater effects of the Imidacloprid than others? 

* Aside from the morphological differences seen between each bee species, we now know different genders of bees have different roles in their colony.  Will we see a trend in gender on the graph??

+ For example: A male social bee (drone), doesn’t go far from the hive unless he is on a mission to find a queen.  A female social bee (worker), does everything in their power to care for their queen and the hive.  They are responsible for producing honey as well as pollinating plants.  With this information would you expect to see a greater effect on the female social bees who have the most direct exposure to plants and the insecticide or male social bees who do not forage much?


What bee species has the highest tolerance to the insecticide? 

With the information regarding the effects of insecticide you can then look at: 

+ What is the background information of this bee?  

+ Does this bee live in a hive or burrow in the ground? 

+ Does the bee stick by the hive or venture out?  


What floral habitats are affected from the use of imidacloprid? 

* For example: Think of the beautiful flowers that sprout up from our natural world.  Is there a trend in the floral habitat that the bee is using?  And if so, could this be due to the concentrations of the insecticide used on this type of plant?  Also, if insecticides are harming our bees making them incapable of pollination, would we expect to see a negative impact on the floral habitats that they occupy? 


Check out the species of bees that are affected from different concentrations of insecticide....

What is their floral specialization ? And what is the type of flower the bee occupies? 


## Methods

**Cleaning the Raw data**

Before creating my shiny app, I created a function that allowed me to take the raw data and upload a clean version at ease every time.  I started by creating a filename with “glue” that corresponded to where my data was saved within my files.  Next, I read in my file using “read_csv”, and within this same step I was able to take care of the NA values in the data.  The raw data contained "." in cells of the table where there was missing information.  I wanted these cells to be recognized as NA values.  Then, I used mutate and “str_replace_all” to clean up and fix specific bee specie names.  Some of the names contaied all uppercase or were abbreviated.

```{r Creating and Reading the data, message=FALSE, warning=FALSE, paged.print=FALSE}

library(readr)
library(glue)
library(stringr)
library(dplyr) # for mutate
library(ggplot2) # library for ggplot
library(hrbrthemes) # for theme

clean_bee_data <- function(a_name){

  # create a file name
  filename <- glue("~/Desktop/data_science/OneDrive - University of Massachusetts Boston/biol_355_2021_fall/final/bee_data/imidacloprid effects on native bees/data/{a_name}.csv")

  # read file name that was created, take care of the NAs
  read_my_file <- read_csv(filename, na = ".") %>%
    mutate(`bee species` =
             str_replace_all(`bee species`, c(`BEMBIX` = "Bembix",
                                              `BICYRTES` = "Bicyrtes",
                                              `D Heteropoda` = "Dieunomia heteropoda",
                                              `M bimaculata` = "Melissodes bimaculata",
                                              `M communis` = "Melissodes communis",
                                              `M xylocopoides` = "Megachile xylocopoides",
                                              `MELISSODES` = "Melissodes",
                                              `MTAUREA` = "Melitoma taurea",
                                              `P bombiformis` = "Ptilothrix bombiformis",
                                              `T concavus` = "Triepeolus concavus",
                                              `X strenua` = "Xenoglossa strenua")))



  return(read_my_file)
}

beetox_data <- clean_bee_data("beetox_data")

```

**How did you decide on the ways you wanted the end-user to be able to explore the data? What information did you chose to give to them and why? What choices did you give them and why?**

First, I wanted the user to be able to “Meet the Bees”.  In this compartment the end user will be able to view an image of a bee first by choosing the species and then by choosing the gender.  This will allow them to be able to compare the morphological differences between the not only each bee species but also the gender between an individual species.  A table was also provided to display the Bee Genus, Level of Sociality and Degree of Floral Specialization of the selected bee species. 

For the Shiny code I used renderImage for my "beeImage" imageOutput.  An ifelse statement was created for the input of the bee gender.  For the filename, glue was used to create a filename based on the input of bee species and gender.  I was not able to locate all the bee species images for every gender, so another ifelse statement was created to account for any missing image.  When no image is available a cartoon bee image is displayed.

```{r Code used for inserting a picture, warning = FALSE, message = FALSE, eval=FALSE}
 # Output for tabPanel "Bee Species" with imageOutput("beeImage )   
output$image <- renderImage({
  
  # if else statement for gender input
  gender <- ifelse(input$gender=="M", "male", "female")

  # When input$species is ***, filename is ****
  filename <- glue("images/{input$species}_{gender}.jpg")
  print(filename)

  if(!file.exists(filename)){
  filename <- "images/no_image.jpg"
  }

  # Return a list containing the filename and alt text
  list(src = filename,
       width = 500,
       height = 400)

  }, deleteFile = FALSE)

```

The bee table in the Shiny app was created using the reactive data.  I started by grouping the data  by "bee genus/species", "Level of Sociality" and "Degree of floral specialization".  Then, I used rename to fix the column names and printed the table by calling the name of the name of the table. 

```{r Table that goes with the selected bee species, warning = FALSE, message = FALSE, tidy=TRUE}

# Create a table that displays bee genus, level of sociality and degree of floral specialization of the selected
the_beetable <- beetox_data %>%

  group_by(`bee genus/species`, `Level of Sociality`,
           `Degree of floral specialization`) %>%
  
  summarise() %>%

  rename(`Bee Genus` = `bee genus/species`,
         `Level of Sociality` = `Level of Sociality`,
         `Degree of Floral Specialization` = `Degree of floral specialization`)

# print bee table 
the_beetable

```

Next, I wanted the user to be able to explore the different angles of the data.  In this compartment of the NavBar the end user will choose a species and then explore different tabsets of plots and tables that coordinate with the chosen species.  In the first tab, “Longevity of Bees” the end user will be able to explore the different effects the insecticide has on bee longevity depending on concentration and bee sex.  A table is also provided in this tab that displays the Bee Species, Sex, and Average longevity based on the floral habitat.  In the following tab labeled “Effects by Floral Habitat” the user will be able to compare the days active versus days paralyzed by concentration of insecticide and the floral habitat that they use.  The purpose being that the user can see if there is a greater effect within one habitat compared to the other. The final tab in this compartment is “Effects by Social Status”.  Here you can compare both the average time paralyzed and average time active (in days) for each bee species.  The plot will clearly highlight and label the selected species while telling whether it is social or solitary at the head of the plot.  By using “gghighlight()” in the making of this plot it allowed for the user to highlight a specified species while keeping all of the others there on the plot, putting the selected species in perspective with the others.

**Layout**

The set up that I choose was a NavBarPanel. I thought that this was the cleanest way to visualize my data.  I liked that you could have main compartments and then tabsets within each one of those compartments.  This made it easier to separate the different angles of the data and what I was trying to represent.

**Visualizations and Analyses**

Within the “Stats” compartment of the Navbar Panel, different tabs are displayed that allow the user to explore the data by different groupings.  With these tabs the user can explore the data and what trends may exist depending on the variable being analyzed.

The first tab is “Sociality”, here the end user can view the average time active by the average time paralyzed of every species, separated by the level of sociality.  I wanted to see if there was any relationship or pattern that existed between social and solitary bees.  A red linear model line was added to the graph surrounded by a confidence interval to highlight any trends that existed within these two groups.

```{r Sociality Code, message=FALSE, warning=FALSE}

 sociality_plot <- beetox_data %>%
  
            # group by bee species and sociality 
            group_by(`bee species`, `Level of Sociality`) %>% 
            
            # create a new column of the average days paralyzed 
            summarize(avg_paralyzed = mean(`days paralyzed`), 
                      
                      # new column of the average days active
                      avg_active = mean(`days active`)) %>% 
  
            # ungroup
            ungroup() %>%
  
            # create a plot 
            ggplot(mapping = aes(x = avg_active, 
                                 y = avg_paralyzed,
                                 color = `bee species`)) + 
            geom_point() + 
  
            # add a linear model line with a confidence interval
            geom_smooth(method=lm , color="red", fill="#69b3a2", se=TRUE) +
  
            # facet_wrap to show plots side by side of different variables 
            facet_wrap(~ `Level of Sociality`) +
  
            # theme
            theme_ipsum() + 
  
            # add labels
            labs(y = "Average Time Paralyzed (days)",
                 x = "Average Time Active (days)",
                 title = "Days Paralyzed versus Days Active",
                 subtitle = "By Level of Sociality",
                 color = "Bee Species")

          # print
          sociality_plot
```

The next tab is "Gender" followed by "Floral Habitat" and "Concentration".  The user can explore the average days active by average days paralyzed grouped by bee sex, floral habitat and concentration of the imidacloprid.  In each of these tabs the data was grouped by the variable of interest and then with facet_wrap I was able to visually diplay the categorized plots side by side.  


```{r warning=FALSE, message=FALSE}

avg_concentration_plot <- beetox_data %>% 
  group_by(`bee species`, `Imidacloprid concentration (parts per billion)`) %>% 
  summarize(avg_paralyzed = mean(`days paralyzed`), 
            avg_active = mean(`days active`)) %>% 
  ungroup() %>%
  ggplot(mapping = aes(x = avg_active, 
                       y = avg_paralyzed,
                       color = `bee species`)) + 
  geom_point() + 
  facet_wrap(~ `Imidacloprid concentration (parts per billion)`) +
    geom_smooth(method=lm , color="red", fill="#69b3a2", se=TRUE) +
  theme_ipsum()

avg_concentration_plot

```


## Results

To conclude the results of the data I decided to focus on the average days paralyzed of each bee species.  I used summarize to create a column of avg_paralyzed which is the average days paralyzed for each bee species.  To visualize this data I used ggplot with bee species as the x axis and avg_paralyzed as the y.  Also, another visual aid that was added to this graph was "fill" for bee species to further distinguish between each species and also "geom_jitter" that displayed black dots of more specific data points on the graph.   

```{r Average Days Paralyzed by Bee Species, warning=FALSE, message=FALSE}

average_paralyzed <- beetox_data %>% 
  group_by(`bee species`, `days paralyzed`) %>% 
  summarize(avg_paralyzed = mean(`days paralyzed`)) %>%
  ungroup() 

avg_paralyzed_plot <- average_paralyzed %>% 
  ggplot(aes(x = `bee species`, 
             y = avg_paralyzed, 
             fill = `bee species`)) + 
  geom_boxplot() + 
  geom_jitter(color="black", size=0.4, alpha=0.9) +
  labs(y = "Average Days Paralyzed",
       x = "Bee Species",
       title = "Days Paralyzed",
       subtitle = "By Bee Species", 
       fill = "Bee Species") +
   theme(axis.text.x = element_text(angle = 90))

avg_paralyzed_plot
```




Using linear models, the average days paralyzed for each bee species was summarized displaying calculations showing the estimated standard error, t value, F value and P value down at the botttom.  

```{r Summary of Average Time Paralyzed and Bee Species, warning=FALSE, message=FALSE}
library(infer)

mod_time <- lm(avg_paralyzed ~ `bee species`, data = average_paralyzed)

summary(mod_time)

#car::Anova(mod_concen)



```

Used another method to calculate the F-statistic to confirm the same results. 
```{r warning=FALSE, message=FALSE}
f_stat <- 
  average_paralyzed %>%
  specify(avg_paralyzed ~ `bee species`) %>%
  hypothesize(null = "independence") %>%
  calculate(stat = "F") 

f_stat
```

```{r warning=FALSE, message=FALSE}
plot(mod_time, which = 2)
```



```{r warning=FALSE, message=FALSE}
null_anova <- 
  average_paralyzed %>%
  specify(avg_paralyzed ~ `bee species`) %>%
  hypothesize(null = "independence") %>% 
  generate(reps = 1000, type = "permute") %>% 
  calculate(stat = "F")

null_anova %>% 
  visualize() +
  shade_p_value(f_stat, direction = "greater")

```



## Discussion 

When discussing results from this lab one has to keep in mind that the data recorded from this study was done in a controlled lab environment.  Any effect experienced by the bee species from this study could differ from those seen in the field.  

From the summary analysis a P-value of 0.005081 was calculated.  This indicates that the days paralyzed by bee species was unlikely to have occurred by chance.

From my app I learned a lot of things that I did not know.  The days paralyzed versus days active grouped by level of sociality does not show any linear trend between the solitary bee species. There was a slight trend in social bees, when days paralyzed is at its highest the days active is at its lowest.  This continues as a declining linear trend in days active versus days paralyzed.  
There was no trend by gender of the bee species or floral habitat.
The days paralyzed versus days active grouped by Imidacloprid Concentration (parts per billion) did show an interesting trend.  The concentration starts off at zero, this shows a horizonal linear line that has the highest number of days active and little to no days paralyzed.  Compared to the highest concentration, 100 parts per billion the average days active is the smallest number of days while days paralyzed is at its largest.  Which indicated that the insecticide imidacloprid did have a negative effect of the bees as the concentration goes up.

The way I set up my app allowed me to not only visualize what the bee species looked like but also what the bee genus, level of sociality and degree of floral specialization was for that bee species.  Also I found that visually seeing how different the male versus female bee species was is pretty interesting.  I was not aware of how different they looked.

If I were to do a follow up study on this data, I would really like to see this same study done in the field so I can compare results.  The amount of control on the study would not be as precise but it would be interesting to see if the results were similar.  When doing the study as is, you must take into consideration that the bees are not operating under normal circumstances.  This could be stressful for the bees in a laboratory setting especially because they would not be able to function as they normally would in nature, foraging for honey and supplies.  Next, I think recording the trend in floral habitats over the years in areas that regularly use insecticides would be a good study to look at next to this.  After analyzing the potentially negative effects of the insecticide on bee species you could see if this is altering the growth patterns of the flowers the occupy.  

Final conclusion that I can draw from the data is that the imidacloprid affects bee species in a negative manor.  But the reality of this situation is unclear and cannot be determined from this data.  Considering that this study was done in a laboratory setting and the concentrations were controlled we cannot say that this is impacting bees in the wild with the same effect.  A field study with actual concentrations used would have to be done to confirm if the insecticide causes a  negative impact on beneficial pollinators and our environment as a whole. 