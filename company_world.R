install.packages('tidyverse')
library('tidyverse')
install.packages('here')
library('here')
install.packages('skimr')
library('skimr')
install.packages('janitor')
library('janitor')
install.packages('dplyr')
library('dplyr')

company_world <- read.csv('C:/Users/Dell/Downloads/companies_sorted.csv')
head(company_world)

# lấy ra các cột cần thiết cho phân tích 
company_world <- subset(company_world, select =c(name, year.founded,industry,size.range,country,current.employee.estimate,total.employee.estimate))
head(company_world)
#kiểm tra xem giá trị null
skim_without_charts(company_world) # biến có giá trị null(year.founded)
# kiểm tra giá trị unique của biến size.range
unique(company_world$size.range)
# đặt giá trị rút ngắn lại

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

#Vẽ biểu đồ histogram khám phá xem dữ liệu của năm thành lập tập trung nhiều ở đâu.
ggplot(data=company_world)+ geom_histogram(mapping = aes(x=year.founded),color='white',fill='black')+
  labs(title = 'Distribution Of Year Of Company Establishment')

# size range
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

# 10 ngành nghề phổ biến nhiều nhất ở các công ty
library(scales)
company_world_1 <- company_world %>%
  count(industry) %>%
  top_n(10, wt = n) %>% filter(industry != '')

ggplot(data=company_world_1)+ geom_col(mapping= aes(x=industry,y=n),color='black')+coord_flip()+ 
labs(title ='Top 10 industries with many companies',
                                          x='Industry',
                                          y='Count') +
  scale_y_continuous(labels = scales::comma, expand = c(0, 0))
# top 10 country có nhiều công ty
library(scales)

company_world_country <- company_world %>%
  count(country) %>%
  slice_max(n, n = 10)%>%
  filter(country != "")
  

ggplot(data=company_world_country)+ geom_col(mapping= aes(x=country,y=n),color='black')+coord_flip()+ 
  labs(title ='Top 10 Country with many companies',
       x='country',
       y='Count') +
  scale_y_continuous(labels = scales::comma, expand = c(0, 0))
#lọc ra những năm 1980 trở về sau
company_world_1880 <- company_world %>%
  filter(year.founded > 1880)
ggplot(company_world_1880)+geom_point(mapping = aes(x=year.founded,y=total.employee.estimate,color=size.range),na.rm = TRUE)+
  scale_y_continuous(labels = scales::comma, expand = c(0, 0))+labs(title ='Correlation between year of establishment variable and  total employees estimate')

#Top 10 quốc gia có số lượng nhân viên nhiều nhất
company_world_employee <- company_world %>% group_by(country)%>% 
  summarise(sum_employee=sum(total.employee.estimate))%>%
  top_n(10, wt = sum_employee)%>% filter(country !='')
  
ggplot(data=company_world_employee)+geom_col(mapping= aes(x=country,y=sum_employee),color='black')+coord_flip()+ 
  labs(title ='Top 10 countries with the most total number of employees',
       x='Country',
       y='Total Employee Estimate') +
  scale_y_continuous(labels = scales::comma, expand = c(0, 0))
#
ggplot(data=company_world)+
  geom_point(mapping = aes(x=size.range,y=current.employee.estimate,color=country),na.rm = TRUE)+
  scale_y_continuous(labels = scales::comma, expand = c(0, 0))+
  labs(title ='Correlation between size range variable and  curent employees estimate by country')



