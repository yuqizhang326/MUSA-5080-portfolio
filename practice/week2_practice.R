library(tidyverse)
library(tidycensus)

#load date

pa_income <- get_acs(
  geography = "county",
  variables = "B19013_001",
  state = "PA",
  year = 2023,
  survey = "acs5"
  )

dim(pa_income)
glimpse(pa_income)
#rows 67 columns 5 It matches with 67 counties in Pennsylvania
head(pa_income)

pa_income$GEOID # I can see all the 67 counties' GEOID, but they are not following the order.

as.numeric("01001") # It drops the first zero. So if GEOIDs are kept in text, it will not have this problem.

filter(pa_income, estimate > 60000) # It will have fewer rows because it filters the counties that income are higher than 60000

# Counties where the margin of error is bigger than 3000
filter(pa_income, moe > 3000)

# Counties where the estimate is under 50000
filter(pa_income, estimate < 50000)

select(pa_income, NAME, estimate, moe) #The row will not change. Filter is for the row and select is for the column

# Show only GEOID and estimate
select(pa_income, GEOID, estimate)

#Make a new column
mutate(pa_income, moe_pct = moe / estimate * 100) #one more column will be made
#actually, nothing appear in the pa_income

moe_pct

pa_income <- mutate(pa_income, moe_pct = moe / estimate * 100)

pa_income
#moe as a percentage of the estimate, how precise it is

arrange(pa_income, moe_pct)
arrange(pa_income, desc(moe_pct)) #will not change the number of rows

step1 <- filter(pa_income, moe_pct > 5)
step2 <- arrange(step1, desc(moe_pct))
step3 <- select(step2, NAME, estimate, moe, moe_pct)
step3

pa_income %>%
  filter(moe_pct > 5) %>%
  arrange(desc(moe_pct)) %>%
  select(NAME, estimate, moe, moe_pct)

# Keep counties with moe_pct over 8, sort by estimate, show NAME and moe_pct
worst <- pa_income %>%
  filter(moe_pct > 8) %>%
  arrange(desc(estimate)) %>%
  select(NAME, moe_pct)

pa_income <- mutate(pa_income, reliable = moe_pct < 5)

pa_income %>%
  group_by(reliable) %>%
  summarize(n = n(),
            avg_income = mean(estimate))

pa_income <- pa_income %>%
  mutate(reliability = case_when(
    moe_pct < 3 ~ "High confidence",
    moe_pct < 6 ~ "Moderate",
    TRUE        ~ "Low confidence"
  ))

count(pa_income, reliability) # High confidence    26 Low confidence      7 Moderate           34


#11
pa_two <- get_acs(
  geography = "county",
  variables = c("B19013_001", "B01003_001"),
  state = "PA", year = 2023, survey = "acs5"
)

pa_two #134 rows, different variables appear twice

pa_wide <- get_acs(
  geography = "county",
  variables = c(income = "B19013_001",
                pop    = "B01003_001"),
  state = "PA", year = 2023, survey = "acs5",
  output = "wide"
)

pa_wide

pa_wide %>%
  mutate(moe_pct = incomeM / incomeE * 100) %>%
  arrange(desc(moe_pct)) %>%
  select(NAME, popE, incomeE, moe_pct) %>%
  head(10)
# Counties with the highest MOE percentages tend to have smaller populations.
# Smaller counties generally have smaller ACS samples, so their estimates are less precise.
