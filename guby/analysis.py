## Import libraries ----
import pandas as pd
import matplotlib.pyplot as plt
import seaborn as sns

## Load the data ----
sales = pd.read_excel("data/SampleData.xlsx", 
                      sheet_name = "SalesOrders")

## View data ----
sales.head()
sales.tail()

## Missing values and duplicates ----
sales.isna().sum()
sales.duplicated().sum()

## Clean column names ----
sales.columns = sales.columns.str.lower()
sales.rename({"unit cost":"unit_cost"}, inplace=True)
sales.head()

## Pairs plot ----
sns.pairplot(sales, corner=True, palette="rocket")
plt.show()

## Pairs plot ----
sales['region'].value_counts().plot(kind = "barh")
plt.title("Sales by Region")
plt.xlabel("Region")
plt.ylabel("Count")
plt.show()

## Data Summary Info ----
sales.info()
sales.describe()
sales.describe(include="object")


## Questions ----
### Total sales by region
sns.pairplot(sales, hue = "region", 
             corner=True, palette="rocket")
plt.show()
sales.groupby("region")["total"].sum().plot(kind="pie")
plt.title("Total Sales by Region")
plt.show()

### Total sales by rep
sns.pairplot(sales, hue = "rep", corner=True, palette="rocket")
plt.show()
sales.groupby("rep")["total"].sum(). sort_values().plot(kind="barh")
plt.title("Total Sales by Sales Rep")
plt.show()

### Total sales by item
sns.pairplot(sales, hue = "item", 
             corner=True, palette="rocket")
plt.show()

sales.groupby("item")["total"].sum(). sort_values().plot(kind="barh")
plt.title("Total Sales by Item")
plt.show()


### Total sales by month
sales["month"] = sales["orderdate"].dt.month
sns.pairplot(sales[["units", "total", "month"]], hue = "month", 
             corner=True, palette="rocket")
plt.show()

sales.head()
sales.groupby("month")["total"].sum(). sort_values().plot(kind="barh")
plt.title("Total Sales by Month")
plt.show()

### Total Sales by Item and Month 
sales.groupby(["month", "item"])["total"].sum().sort_values().plot(kind="barh")
plt.title("Total Sales by Item and Month")
plt.show()
