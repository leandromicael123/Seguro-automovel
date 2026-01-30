library(psych)
library(corrplot)
library(dplyr)
library(cluster)
library(tidyr)
library(ggplot2)

# Dataset ----

  #Read the dataset
  dataset <- read.csv(file.choose(), header=TRUE, sep=",", stringsAsFactors=FALSE)
  
  dataset <- dataset[dataset$PLGR03 == 1, ]
  dataset <- cbind(dataset[1:24])



# Clean the dataset ----
  dataset[dataset == ""] <- NA
  dataset[dataset == "?"] <- NA
  
  # Deal with missing values
  print("Number of missing values before cleaning")
  print(sum(colSums(is.na(dataset))))
  print(colSums(is.na(dataset)))

  # Convert to data frame for plotting
  missing_df <- data.frame(
    Column = names(colSums(is.na(dataset))),
    MissingValues = as.numeric(colSums(is.na(dataset)))
  )
  
  # Keep only columns with at least one missing value
  missing_df <- missing_df %>% filter(MissingValues > 0)
  
  # Bar plot of missing values per column
  ggplot(missing_df, aes(x = reorder(Column, -MissingValues), y = MissingValues, fill = Column)) +
    geom_bar(stat = "identity") +
    labs(title = "Number of Missing Values per Column",
         x = "Columns",
         y = "Number of Missing Values") +
    theme_minimal() +
    theme(legend.position = "none")
  
  # Deal with the missing values
  dataset$collision_type[is.na(dataset$collision_type)] <- "No Collision"
  dataset$property_damage[is.na(dataset$property_damage)] <- "UNKNOWN"
  dataset$police_report_available[is.na(dataset$police_report_available)] <- "UNKNOWN"
  dataset$authorities_contacted[is.na(dataset$authorities_contacted)] <- "No authorities contacted"
  print("Number of missing values after cleaning")
  print(sum(colSums(is.na(dataset))))



# Clean the outliers ----
  # Find the outliers
  resumo_outliers <- function(x) {
    Q1 <- quantile(x, 0.25, na.rm = TRUE)
    Q3 <- quantile(x, 0.75, na.rm = TRUE)
    IQR <- Q3 - Q1
    
    limite_inferior <- Q1 - 1.5 * IQR
    limite_superior <- Q3 + 1.5 * IQR
    
    if (limite_inferior < 0){
      limite_inferior = 0
    }
    
    outliers <- x[x < limite_inferior | x > limite_superior]
    
    list(
      Q1 = Q1,
      Q3 = Q3,
      IQR = IQR,
      Limite_Inferior = limite_inferior,
      Limite_Superior = limite_superior,
      Outliers = outliers
    )
  }
  
  dataset_num <- cbind(dataset[14], dataset[,16:17], dataset[,19:22])
  
  resultados <- lapply(dataset_num, resumo_outliers)
  
  # Make a table with the results
  tabela <- lapply(resultados, function(x) {
    data.frame(
      Q1 = as.numeric(x$Q1),
      Q3 = as.numeric(x$Q3),
      IQR = as.numeric(x$IQR),
      Limite_Inferior = as.numeric(x$Limite_Inferior),
      Limite_Superior = as.numeric(x$Limite_Superior),
      Outliers = paste(x$Outliers, collapse = ", ")
    )
  }) %>%
    bind_rows(.id = "Variaveis")  # adiciona uma coluna com o nome da coluna original
  
  # Check results
  tabela
  
  # Passar para formato longo
  claims_long <- pivot_longer(
    dataset[c("total_claim_amount", "property_claim", "vehicle_claim")],
    cols = everything(),
    names_to = "Tipo_Claim",
    values_to = "Valor"
  )
  
  ggplot(claims_long, aes(x = Tipo_Claim, y = Valor, fill = Tipo_Claim)) +
    geom_boxplot(alpha = 0.7, outlier.alpha = 0.4) +
    labs(
      
      x = "Tipo de indemnização",
      y = "Valor"
    ) +
    theme_minimal() +
    theme(
      legend.position = "none",
      axis.text.x = element_text(angle = 20, hjust = 1)
    )
  ggsave(
    filename = "boxplot.png",
    width = 8,
    height = 6,
    dpi = 300
  )
  
  # Delete the outliers
  for (coluna in names(dataset_num)) {
    limite_inf <- resultados[[coluna]]$Limite_Inferior
    limite_sup <- resultados[[coluna]]$Limite_Superior
    
    # Replace the outliers for NA and then remove the lines
    dataset[[coluna]][dataset[[coluna]] < limite_inf |
                           dataset[[coluna]] > limite_sup] <- NA
    
    dataset <- na.omit(dataset)
  }



# Change categorical values to numeric and rearrange the dataset ----
  #Change the "incident_severity" column to numeric
  dataset$incident_severity[dataset$incident_severity == "Trivial Damage"] <- 1
  dataset$incident_severity[dataset$incident_severity == "Minor Damage"] <- 2
  dataset$incident_severity[dataset$incident_severity == "Major Damage"] <- 3
  dataset$incident_severity[dataset$incident_severity == "Total Loss"] <- 4
  dataset$incident_severity <- as.numeric(dataset$incident_severity)
  
  
  #Change the "incident_type" column to numeric
  #dataset$Single_Vehicle_Collision <- ifelse(dataset$incident_type=="Single Vehicle Collision", 1, 0)
  dataset$Multi_vehicle_Collision <- ifelse(dataset$incident_type=="Multi-vehicle Collision", 1, 0)
  #dataset$Vehicle_Theft <- ifelse(dataset$incident_type=="Vehicle Theft", 1, 0)
  dataset$Parked_Car <- ifelse(dataset$incident_type=="Parked Car", 1, 0)
  
  
  #Change the "collision_type" column to numeric
  dataset$Collision <- ifelse(dataset$collision_type=="No Collision", 0, 1)
  
  
  #Change the "authorities_contacted" column to numeric
  dataset$Police <- ifelse(dataset$authorities_contacted=="Police", 1, 0)
  dataset$Ambulance <- ifelse(dataset$authorities_contacted=="Ambulance", 1, 0)
  dataset$Fire <- ifelse(dataset$authorities_contacted=="Fire", 1, 0)
  #dataset$Other <- ifelse(dataset$authorities_contacted=="Other", 1, 0)
  dataset$No_authorities <- ifelse(dataset$authorities_contacted=="No authorities contacted", 1, 0)
  
  
  #Swap the "incident_hour_of_the_day" column with "incident_type"
  temp <- dataset$incident_hour_of_the_day
  dataset$incident_hour_of_the_day <- dataset$incident_type
  dataset$incident_type <- temp
  colnames(dataset)[9] <- "incident_hour_of_the_day"
  colnames(dataset)[13] <- "incident_type"
  
  
  #Swap the "property_damage" column with "collision_type"
  temp <- dataset$property_damage
  dataset$property_damage <- dataset$collision_type
  dataset$collision_type <- temp
  colnames(dataset)[10] <- "property_damage"
  colnames(dataset)[15] <- "collision_type"
  
  
  #Swap the "police_report_available" column with "incident_severity"
  temp <- dataset$police_report_available
  dataset$police_report_available <- dataset$incident_severity
  dataset$incident_severity <- temp
  colnames(dataset)[11] <- "police_report_available"
  colnames(dataset)[18] <- "incident_severity"
  
  
  #Swap the "bodily_injuries" column with "authorities_contacted"
  temp <- dataset$bodily_injuries
  dataset$bodily_injuries <- dataset$authorities_contacted
  dataset$authorities_contacted <- temp
  colnames(dataset)[12] <- "bodily_injuries"
  colnames(dataset)[16] <- "authorities_contacted"
  
  
  #Swap the "witnesses" column with "incident_type"
  temp <- dataset$witnesses
  dataset$witnesses <- dataset$incident_type
  dataset$incident_type <- temp
  colnames(dataset)[13] <- "witnesses"
  colnames(dataset)[17] <- "incident_type"
  
  
  #Swap the "auto_make" column with "number_of_vehicles_involved"
  temp <- dataset$auto_make
  dataset$auto_make <- dataset$number_of_vehicles_involved
  dataset$number_of_vehicles_involved <- temp
  colnames(dataset)[14] <- "auto_make"
  colnames(dataset)[23] <- "number_of_vehicles_involved"
  
  
  #Swap the "auto_year" column with "collision_type"
  temp <- dataset$auto_year
  dataset$auto_year <- dataset$collision_type
  dataset$collision_type <- temp
  colnames(dataset)[15] <- "auto_year"
  colnames(dataset)[24] <- "collision_type"
  
  
  
  dataset <- cbind(dataset[, 1:15], dataset[18], dataset[20:23], dataset[,25:ncol(dataset)])
  
  
  #Characteristics
  head(dataset)



# Correlations ----
#Correlation plot with colors (too many attributes)
correlation <- cor(dataset[,16:ncol(dataset)])
corrplot(correlation)
round(correlation, 3)



# Verificar os PCAs ----

#Bartlett test
cortest.bartlett(correlation)


#KMO measure 
KMO(correlation)

#Remove the variabels with low MSA (MSA < 0.5)
ignore <- c("Police", "Ambulance", "Fire")
dataset <- dataset[, !(names(dataset) %in% ignore)]

#Check the KMO measure again
correlation <- cor(dataset[,16:ncol(dataset)])
corrplot(correlation)

KMO_result <- KMO(correlation)
msa_var <- KMO_result$MSAi
msa_df <- data.frame(
  Variavel = names(msa_var),
  MSA = as.numeric(msa_var)
)
ggplot(msa_df, aes(x = reorder(Variavel, MSA), y = MSA)) +
  geom_col(fill = "steelblue", alpha = 0.8) +
  coord_flip() +
  geom_hline(yintercept = 0.5, linetype = "dashed", colour = "red") +
  labs(
    title = "Medida de Adequação da Amostra (MSA) por variável",
    x = "Variável",
    y = "MSA"
  ) +
  theme_minimal()

#Scalling of attributes data
data_scaled <- scale(dataset[,16:ncol(dataset)])


#Assume the number of components = 8   (ncol(data_scaled) = 8)
pc8 <- principal(data_scaled, nfactors=8, rotate="none")

#Screeplot - Find the elbow
plot(pc8$values, type = "b", main = "Scree plot for Insurance dataset",
     xlab = "Number of PC", ylab = "Eigenvalue")

#Eigenvalues - Variances of the principal components 
#Kaiser criterion
plot(round(pc8$values,3))

#Variância explicada (Cumulative var)
pc8$loadings

pc3 <- principal(data_scaled, nfactors=3, rotate="promax")
round(pc3$communality, 3)
pc3$loadings



# PCAs ----

pc_final <- principal(data_scaled, nfactors=3, rotate="none")
round(pc_final$communality, 3)
pc_final$loadings

dataset$Indenimizacoes <- pc_final$scores[,1] # PCA 1
dataset$Numero_veiculos <- pc_final$scores[,2] # PCA 2
dataset$Gravidade_acidentes <- pc_final$scores[,3] # PCA 3

#Plot of the PCAs
pairs(dataset[,25:27], pch = 19, lower.panel = NULL)



# Criar os clusters ----
  #Remove the input variables
  dataset <- cbind(dataset[,1:15], dataset[,25:ncol(dataset)])
  head(dataset)
  
  #Criar os clusters e adiciona-los no dataset
  pca_data <- dataset[, c("Indenimizacoes", "Numero_veiculos", "Gravidade_acidentes")]
  pc_dist <- dist(pca_data, method = "manhattan")
  hc <- hclust(pc_dist, method = "average")
  idx_ok <- complete.cases(pca_data)
  
  #Número de clusters (k)
  k <- 3
  
  #Dendrograma
  plot(hc, hang = -1, labels = FALSE, main = "Dendrograma (average linkage) - Clustering nos PCAs", xlab = "", sub = "")
  rect.hclust(hc, k = k, border = "red")
  
  #Guardar no dataset, respeitando as linhas removidas por NA
  cluster_hc <- cutree(hc, k = k)
  dataset$cluster_hc[idx_ok] <- cluster_hc
  dataset$cluster_hc <- NA
  dataset$cluster_hc <- cluster_hc
  
  #Silhouette
  sil <- silhouette(cluster_hc, pc_dist)
  plot(
    sil,
    col = rainbow(k),
    border = NA,
    main = "Silhouette – average linkage nos PCAs"
  )
  mean(sil[, 3])


  
# Legendas do clusters ----
  #Acidentes simples -> AS
  dataset$cluster_hc[dataset$cluster_hc == 1] <- "AS"
  
  #Acidentes complexo com alto custo -> ACAC
  dataset$cluster_hc[dataset$cluster_hc == 2] <- "ACAC"
  
  #Acidentes com baixo custo -> ABC
  dataset$cluster_hc[dataset$cluster_hc == 3] <- "ABC"
  
  
  
# Analise dos clusters ----
  dataset_long <- dataset %>%
    pivot_longer(
      cols = c(Indenimizacoes, Numero_veiculos, Gravidade_acidentes),
      names_to = "PC",
      values_to = "value"
    )
  
  ggplot(dataset_long,
         aes(x = factor(cluster_hc),
             y = value,
             fill = factor(cluster_hc))) +
    geom_boxplot(show.legend = FALSE) +
    facet_wrap(~ PC, scales = "free_y") +
    labs(
      title = "Distribuição dos PCs por cluster",
      x = "Cluster",
      y = "Valor do PC"
    ) +
    theme_minimal()
  
  
  
  centroids <- dataset %>%
    group_by(cluster_hc) %>%
    summarise(
      Indenimizacoes = mean(Indenimizacoes),
      Numero_veiculos = mean(Numero_veiculos),
      Gravidade_acidentes = mean(Gravidade_acidentes)
    )
  
  ggplot(dataset,
         aes(Numero_veiculos, Indenimizacoes, color = factor(cluster_hc))) +
    geom_point(alpha = 0.3) +
    geom_point(data = centroids,
               aes(Numero_veiculos, Indenimizacoes),
               size = 6,
               shape = 4,
               stroke = 2,
               color = "black") +
    labs(
      title = "Clusters e centróides (PC1 vs PC2)",
      x = "PC2 – Numero_veiculos",
      y = "PC1 – Indenimizacoes"
    ) +
    theme_minimal()
  
  ggplot(dataset,
         aes(Gravidade_acidentes, Indenimizacoes, color = factor(cluster_hc))) +
    geom_point(alpha = 0.3) +
    geom_point(data = centroids,
               aes(Gravidade_acidentes, Indenimizacoes),
               size = 6,
               shape = 4,
               stroke = 2,
               color = "black") +
    labs(
      title = "Clusters e centróides (PC1 vs PC3)",
      x = "PC3 – Gravidade_acidentes",
      y = "PC1 – Indenimizacoes"
    ) +
    theme_minimal()
  
  ggplot(dataset,
         aes(Gravidade_acidentes, Numero_veiculos, color = factor(cluster_hc))) +
    geom_point(alpha = 0.3) +
    geom_point(data = centroids,
               aes(Gravidade_acidentes, Numero_veiculos),
               size = 6,
               shape = 4,
               stroke = 2,
               color = "black") +
    labs(
      title = "Clusters e centróides (PC2 vs PC3)",
      x = "PC3 – Gravidade_acidentes",
      y = "PC2 – Numero_veiculos"
    ) +
    theme_minimal()



# Gráfico dos meses como clientes ----
  ggplot(dataset, aes(x = factor(cluster_hc), y = months_as_customer
                      , fill = factor(cluster_hc))) +
    geom_boxplot() +
    labs(
      title = "Boxplot dos meses por cliente por cluster",
      x = "Cluster",
      y = "Meses como cliente"
    ) +
    theme_minimal()


  
# Gráfico das idades dos clientes ----
  ggplot(dataset, aes(x = factor(cluster_hc), y = age
                      , fill = factor(cluster_hc))) +
    geom_boxplot() +
    labs(
      title = "Boxplot das idades dos cliente por cluster",
      x = "Cluster",
      y = "Idades"
    ) +
    theme_minimal()
  
  
# Gráfico do género dos clientes ----
  ggplot(dataset, aes(x = factor(cluster_hc), fill = insured_sex)) +
    geom_bar(position = "fill") +
    labs(
      title = "Distribuição do género por cluster",
      x = "Cluster",
      y = "Proporção",
      fill = "Género"
    ) +
    scale_y_continuous(labels = scales::percent_format()) +
    theme_minimal()
  
  

# Gráfico do nivel da educação ----
  ggplot(dataset, aes(x = cluster_hc, fill = insured_education_level)) +
    geom_bar(position = "fill") +
    labs(
      title = "Distribuição do nível de educação por cluster",
      x = "Cluster",
      y = "Proporção",
      fill = "Nível de educação"
    ) +
    scale_y_continuous(labels = scales::percent_format()) +
    theme_minimal() +
    theme(
      axis.text.x = element_text(angle = 25, hjust = 1)
    )
  
  
# Gráfico das ocupações dos clientes ----
    #Primeiro é agrupar os valores
  dataset <- dataset %>%
    mutate(insured_occupation_grouped = case_when(
      insured_occupation %in% c("exec-managerial", "prof-specialty", "tech-support", "adm-clerical", "sales") ~ "Cognitive jobs",
      TRUE ~ "Physical Jobs"
    ))
  
  ggplot(dataset, aes(x = cluster_hc, fill = insured_occupation_grouped)) +
    geom_bar(position = "fill") +
    labs(
      title = "Distribuição das ocupações dos clientes por cluster",
      x = "Cluster",
      y = "Proporção",
      fill = "Ocupações"
    ) +
    scale_y_continuous(labels = scales::percent_format()) +
    theme_minimal() +
    theme(
      axis.text.x = element_text(angle = 25, hjust = 1)
    )
  
  
  
# Gráfico dos hobbies dos clientes ----
  #Primeiro é agrupar os valores
  dataset <- dataset %>%
    mutate(insured_hobbies_grouped = case_when(
      insured_hobbies %in% c("sleeping", "movies", "yachting") ~ "Relaxing hobbies",
      insured_hobbies %in% c("board-games", "reading", "chess", "video-games") ~ "Intellectual hobbies",
      TRUE ~ "Physical hobbies"
    ))
  
  #Depois podemos fazer o gráfico
  ggplot(dataset, aes(x = cluster_hc, fill = insured_hobbies_grouped)) +
    geom_bar(position = "fill") +
    labs(
      title = "Distribuição dos hobbies por cluster",
      x = "Cluster",
      y = "Proporção",
      fill = "Tipos de hobbies"
    ) +
    scale_y_continuous(labels = scales::percent_format()) +
    theme_minimal() +
    theme(
      axis.text.x = element_text(angle = 25, hjust = 1)
    )
  
  
  
# Gráfico das relações familiares dos clientes ----  
  ggplot(dataset, aes(x = cluster_hc, fill = insured_relationship)) +
    geom_bar(position = "fill") +
    labs(
      title = "Distribuição das relações familiares por cluster",
      x = "Cluster",
      y = "Proporção",
      fill = "Tipos de relações"
    ) +
    scale_y_continuous(labels = scales::percent_format()) +
    theme_minimal() +
    theme(
      axis.text.x = element_text(angle = 25, hjust = 1)
    )



# Gráfico das datas dos acidentes ----
  dataset$incident_date <- as.Date(dataset$incident_date)
  
  summary_df <- dataset %>%
    group_by(incident_date, cluster_hc) %>%
    summarise(count = n(), .groups = "drop")
  
  ggplot(summary_df, aes(x = incident_date, y = count, color = cluster_hc, group = cluster_hc)) +
    geom_line(size = 1.5) +
    geom_point() +
    labs(x = "Month", y = "Count", color = "Cluster") +
    scale_x_date(date_breaks = "1 month", date_labels = "%b") +  # only month abbreviations
    theme_minimal()



# Gráfico das horas dos acidentes ----  
  summary_df <- dataset %>%
    group_by(incident_hour_of_the_day, cluster_hc) %>%
    summarise(count = n(), .groups = "drop")
  
  ggplot(summary_df, aes(x = incident_hour_of_the_day, y = count, color = cluster_hc, group = cluster_hc)) +
    geom_line(size = 1.5) +
    geom_point() +
    theme_minimal()
  


# Gráfico do dano das propriedade ---- 
  ggplot(dataset, aes(x = cluster_hc, fill = property_damage)) +
    geom_bar(position = "fill") +
    labs(
      title = "Existencia de danos nas propriedades por cluster",
      x = "Cluster",
      y = "Proporção",
      fill = "Dano nas propriedades"
    ) +
    scale_y_continuous(labels = scales::percent_format()) +
    theme_minimal() +
    theme(
      axis.text.x = element_text(angle = 25, hjust = 1)
    )



# Gráfico da disponibilidade de reportes policiais ---- 
  ggplot(dataset, aes(x = cluster_hc, fill = police_report_available)) +
    geom_bar(position = "fill") +
    labs(
      title = "Disponibilidade de relatório policial por cluster",
      x = "Cluster",
      y = "Proporção",
      fill = "Relatório policial"
    ) +
    scale_y_continuous(labels = scales::percent_format()) +
    theme_minimal() +
    theme(
      axis.text.x = element_text(angle = 25, hjust = 1)
    )



# Gráfico do número de vitimas de feridas ----
  ggplot(dataset, aes(x = factor(cluster_hc), y = mean(bodily_injuries))) +
    geom_col(fill = "steelblue") +
    labs(
      title = "Número médio de vítimas feridas por cluster",
      x = "Cluster",
      y = "Soma de vítimas feridas"
    ) +
    theme_minimal()

  
  
# Gráfico do número de testemunhas ----
  ggplot(dataset, aes(x = factor(cluster_hc), y = mean(witnesses))) +
    geom_col(fill = "darkorange") +
    labs(
      title = "Número médio de testemunhas por cluster",
      x = "Cluster",
      y = "Soma de testemunhas"
    ) +
    theme_minimal()



# Gráfico do ano do carro envolvido no acidente ----
  summary_df <- dataset %>%
    group_by(auto_year, cluster_hc) %>%
    summarise(count = n(), .groups = "drop")
  
  ggplot(summary_df, aes(x = auto_year, y = count, color = cluster_hc, group = cluster_hc)) +
    geom_line(size = 1.5) +
    geom_point() +
    theme_minimal()
  
  
  
# Gráfico da marca do carro envolvido no acidente ----
  #Primeiro é agrupar os valores
  dataset <- dataset %>%
    mutate(auto_make_grouped = case_when(
      auto_make %in% c("Toyota", "Ford", "tech-support", "Chevrolet", "Nissan", "Honda") ~ "Low maintenance",
      auto_make %in% c("Jeep", "Volkswagen") ~ "Medium maintenance",
      auto_make %in% c("Suburu", "Saab", "Accura", "BMW") ~ "High maintenance",
      TRUE ~ "Very high maintenance"
    ))
  
  ggplot(dataset, aes(x = cluster_hc, fill = auto_make_grouped)) +
    geom_bar(position = "fill") +
    labs(
      title = "Distribuição do preço de manutenção dos carros por cluster",
      x = "Cluster",
      y = "Proporção",
      fill = "Custo de manutenção dos carros"
    ) +
    scale_y_continuous(labels = scales::percent_format()) +
    theme_minimal() +
    theme(
      axis.text.x = element_text(angle = 25, hjust = 1)
    )


