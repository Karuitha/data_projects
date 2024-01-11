# Import important packages
import pandas as pd
import matplotlib.pyplot as plt

# Read in the datase
my_who_data = pd.read_csv("datasets/world_health_organization.csv")

my_who_data = my_who_data.rename(columns={"CHE GDP(%)": 'Percent_change_gdp'})
my_who_data["Continent"].unique()

continent_code = []

for continent in my_who_data['Continent']:
    if continent == "Asia":
        continent_code.append(1)
    elif continent == "Europe":
        continent_code.append(2)
    elif continent == "Africa":
        continent_code.append(3)
    elif continent == "South America":
        continent_code.append(4)
    elif continent == "North America":
        continent_code.append(5)
    elif continent == "South Oceania":
        continent_code.append(6)
    else:
        continent_code.append(7)

print(continent_code)

my_who_data['continent_code'] = continent_code


# Explore the data set
my_who_data.head()
my_who_data.info()
my_who_data.shape

# Check for missing data
my_who_data.isnull().sum()
#my_who_data.

# The target variable is life expectancy
plt.scatter(my_who_data['Life_expectancy'],
            my_who_data['BMI'],
            c=my_who_data['continent_code'],
            s=my_who_data['Population'] / 1000000)

plt.xlabel("Life Expectancy")
plt.ylabel("BMI")
plt.title("BMI versus Life Expectancy", fontsize=30)
plt.legend()
# plt.gray()

plt.show()

my_who_data.boxplot(column=['Life_expectancy'], by=['Continent'])
plt.xlabel = ''
plt.ylabel = ''
plt.show()

my_who_data.boxplot(column=['LifeExp_female'], by=['Continent'])
plt.show()

# Compute numeric summaries
my_who_data.groupby('Continent').median().sort_values('Life_expectancy')[
    ['BMI', 'Population', 'LifeExp_female', 'Life_expectancy', 'Percent_change_gdp', 'GDP']]
