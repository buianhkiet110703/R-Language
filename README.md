# R-Language
## Introduce Dataset
The dataset contains information of companies established worldwide (7173426 rows,9 columns) including the size of the company, the year it was founded, and industries around the world.

![image](https://user-images.githubusercontent.com/110837675/225264416-562aae34-f917-4280-960d-bc90d613331f.png)

- Name : Name of Company

- Domain : Domain of Company

- Year founded : Year company was established/founded

- Industry : Industry company belongs to

- Size range : Size range of company defined by total staff/number of employees

- Locality : Location of company including city, state, country

- Country : Country in which company resides

- Linkedin url : LinkedIn URL associated with company

- Current employee estinate : Current number of employees at company

With this dataset, I will explore which industries are the most developed and of interest to many companies, which countries have the most companies and are most developed, as well as how many employees each company has.

## II. Data Cleaning 

I will filter the columns I need for analysis. (name, year.founded,industry,size.range,country,current.employee.estimate,total.employee.estimate)
```php
company_world <- subset(company_world, select =c(name, year.founded,industry,size.range,country,current.employee.estimate,total.employee.estimate))
```
I will check out the missing value and explore the type of column.

![image](https://user-images.githubusercontent.com/110837675/225284997-0319a440-2b98-492b-8c1d-b7be77f84c42.png)

The year Founded column has 3606980 missing values. The company before 1880 doesn't have information, so I will explore the companys one after 1800.

The year Founded column has 3606980 missing values. The company before 1880 doesn't have information, so I will explore the companys one after 1800 and convert it into factor values.
```php
company_world <- company_world %>% 
  mutate(size.range = case_when(
    size.range %in% c('1 - 10') ~ 1,
    size.range %in% c('11 - 50') ~ 2,
    size.range %in% c('51 - 200') ~ 3,
    size.range %in% c('201 - 500') ~ 4,
    size.range %in% c('501 - 1000') ~ 5,
    size.range %in% c('1001 - 5000') ~ 6,
    size.range %in% c('5001 - 10000') ~ 7,
    TRUE ~ 8
  ))
company_world$size.range <- as.factor(company_world$size.range)
```
## III. Visualization

1.I will draw a histogram to see a distribution of the year founded.
```php
ggplot(data=company_world)+ geom_histogram(mapping = aes(x=year.founded),color='white',fill='black')+
  labs(title = 'Distribution Of Year Of Company Establishment')
```
![image](https://user-images.githubusercontent.com/110837675/225288222-19759ded-48fd-4219-b295-0039c780d0a0.png)

We can observe the distribution of the year of establishment of companies. From 2000 onwards is the period with the highest concentration of data, and also the time when most companies were established.

=> The global trend started to develop strongly from the year 2000 onwards.

2. I will draw a scatter plot to show the correlation between the "year founded" variable and the "total employee estimate" variable, to see how they correlate with each other.
```php
company_world_1880 <- company_world %>%
  filter(year.founded > 1880)
ggplot(company_world_1880)+geom_point(mapping = aes(x=year.founded,y=total.employee.estimate,color=size.range),na.rm = TRUE)+
  scale_y_continuous(labels = scales::comma, expand = c(0, 0))+labs(title ='Correlation between year of establishment variable and  total employees estimate')
```


![](https://scontent.fsgn2-4.fna.fbcdn.net/v/t1.15752-9/336181910_3360435727503096_2599528537322727237_n.png?_nc_cat=101&ccb=1-7&_nc_sid=ae9488&_nc_ohc=DhOPKeJzKJ0AX9JfJVH&_nc_ht=scontent.fsgn2-4.fna&oh=03_AdTBqbbydojO3qxmc-16lZPml4prac0V7JjR009q8Mvlsg&oe=6439015E)

This is a chart illustrating the correlation between the year founded and the total number of employees working for the company, and the color in the chart represents the size of the company. The chart shows us that the correlation between these two variables is positive, the smaller the size of the company, the fewer employees there are, and the larger the size of the company, the more employees there are working for that company.

3. I will create a chart to explore the size distribution of companies, which size has the majority, by using a pie chart.

```php
company_world_size <- company_world %>%
  count(size.range)
ggplot(data = company_world_size, aes(x = "", y = n, fill = size.range, label = paste0(size.range, "\n", scales::percent(n/sum(n))))) + 
  geom_bar(width = 1, stat = "identity") +
  coord_polar(theta = "y") +
  scale_color_manual(values = c("#FF0000", "#00FF00", "#0000FF", "#FFFF00", "#00FFFF", "#FF00FF", "#C0C0C0", "#800000")) +
  ggtitle("Size Range of Companies Worldwide") +
  theme_void() +
  theme(plot.title = element_text(hjust = 0.5)) +
  geom_text(position = position_stack(vjust = 0.5), size = 4, check_overlap = TRUE)
```

![image](https://user-images.githubusercontent.com/110837675/225295880-61c32493-de04-45fd-917d-3f41f2591e58.png)

As I converted the size of the company into a more visually readable format using the code above, the size of the company will increase from 1 to 8. The chart clearly shows the size distribution of the companies, with size 1 accounting for the highest proportion at 77.8%, followed by size 2 at 15.7%. Companies with larger sizes are much less common.

Since 2000 is a relatively short period of time and a large number of companies have been established during this time, the small size of companies is likely due to their early stages of development and limited initial investment, resulting in a lower number of employees. Therefore, we can see that there are few large companies in the world, and most of them are small companies with sizes ranging from 1-1000.

=> This indicates that there is a significant trend of entrepreneurship around the world, and the world is likely to develop rapidly in the future.

4. Next, I will examine which countries have the highest number of companies.
```php
library(scales)

company_world_country <- company_world %>%
  count(country) %>%
  slice_max(n, n = 10)%>%
  filter(country != "")
  

ggplot(data=company_world_country)+ geom_col(mapping= aes(x=country,y=n),color='black',fill='yellow')+coord_flip()+ 
  labs(title ='Top 10 Country with many companies',
       x='country',
       y='Count') +
  scale_y_continuous(labels = scales::comma, expand = c(0, 0))
```

![image](https://user-images.githubusercontent.com/110837675/225299648-3a6206c0-0e2b-4582-bff4-90c39952e58a.png)

Surprisingly, the United States is the country with the highest number of companies in the world with over 2,000,000 companies, far ahead of other countries.

=>This shows that the United States is the most developed country in the world with the most companies. In the near future, the United States will surely become the center of attracting talent from around the world because of their large number of companies. Currently, the United States is the richest country in the world, which has been proven through this chart.

5. I will draw a chart showing the current number of employees working for companies by country.
```php
company_world_employee <- company_world %>% group_by(country)%>% 
  summarise(sum_employee=sum(total.employee.estimate))%>%
  top_n(10, wt = sum_employee)%>% filter(country !='')
  
ggplot(data=company_world_employee)+geom_col(mapping= aes(x=country,y=sum_employee),color='black')+coord_flip()+ 
  labs(title ='Top 10 countries with the most total number of employees',
       x='Country',
       y='Total Employee Estimate') +
  scale_y_continuous(labels = scales::comma, expand = c(0, 0))
```
![image](https://user-images.githubusercontent.com/110837675/225301471-f099e561-fd7e-4645-9ae9-06c94c4be34e.png)

Not surprisingly, with the above chart, we can also predict that the United States will be the country that attracts the most resources, because they have a large number of companies and therefore require a lot of labor to meet their needs. The US economy has a very high output of many products.

6.I will explore which industries are most focused on by companies worldwide

```php
library(scales)
company_world_1 <- company_world %>%
  count(industry) %>%
  arrange(desc(n)) %>%top_n(10, wt = n)

ggplot(data=company_world_1)+ geom_col(mapping= aes(x=industry,y=n),color='black')+coord_flip()+ 
labs(title ='Top 10 industries with many companies',
                                          x='Industry',
                                          y='Count') +
  scale_y_continuous(labels = scales::comma, expand = c(0, 0))
```

![image](https://user-images.githubusercontent.com/110837675/225304466-bc542a9b-b3fe-43c6-a53e-a92fc6f37234.png)

Here are the top 10 industries that companies around the world focus on the most. Leading the list is the technology industry. In the current era of Industry 4.0, companies are concentrating on technology-related fields, as technology is the fastest-developing factor. It is because the United States has technology companies that have made it the world's number one. Nowadays, technology can determine the survival of a company, so companies worldwide are strongly focusing on this industry. Next on the list is marketing and advertising, construction...

## IV. Conclusion
Firstly, we can see that the world trend is to establish more and more companies, which indicates that the world will be even more developed in the future.

Secondly, in the dataset, we can see that the United States has the most companies, is the most developed country, and has far more companies than other countries. Therefore, we can learn about how American companies have grown to such a large scale and use this as a reference for ourselves.

Thirdly, we can see that the technology sector is the one that companies around the world are focusing on developing the most. Therefore, we need to pay more attention to this trend to avoid being left behind by the world's trend.














  

