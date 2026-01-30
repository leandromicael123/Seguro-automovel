# Seguro-automovel
Este repositório contém o trabalho desenvolvido no âmbito da unidade curricular Métodos de Aprendizagem Não Supervisionada da Licenciatura em Ciência de Dados (ISCTE-IUL). O projeto aplica Análise em Componentes Principais (PCA) e clustering hierárquico à análise de sinistros automóveis, com o objetivo de identificar tipologias homogéneas de acidentes e apoiar a tomada de decisão em contexto segurador.

A análise baseia-se numa amostra de sinistros automóveis, envolvendo variáveis quantitativas e qualitativas relativas ao segurado, ao incidente, ao veículo e às indemnizações. O workflow inclui tratamento de valores omissos, remoção de outliers, codificação de variáveis categóricas, seleção de variáveis com base em KMO e teste de Bartlett, redução de dimensionalidade via PCA com rotação oblíqua (Promax) e posterior clustering hierárquico sobre os scores das componentes principais.

O resultado final identifica três clusters interpretáveis — Acidentes Simples, Acidentes Complexos com Alto Custo e Acidentes com Baixo Custo — evidenciando padrões estruturais relevantes para gestão de risco, pricing e eficiência operacional em seguradoras.

O repositório inclui código, outputs gráficos e o relatório final, permitindo reproduzir integralmente a análise e servir como referência para projetos de segmentação e análise exploratória em ciência de dados aplicada.
