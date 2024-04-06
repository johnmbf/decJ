# decJ 1.0.0.900X

## Correções

* Correção da documentação das funções. Algumas funções estavam com exemplo impossível de funcionar pois não respeitavam os parâmetros. Criei também famílias de funções.

* Os dados que antes eram acessíveis pelo pacote se tornaram internos. Eles ainda podem ser acessados dentro de `R/sysdata.rda`.

## Novas funções

* Nova `alepr_projeto()` permite que você extraia do site da Assembleia Legislativa do Paraná informações sobre proposições legislativas a partir de uma legislação já aprovada.

* Nova `utilitario_tscolor()` permite que você crie uma paleta de cores para gráficos utilizando de cores referente as capas dos albuns da Taylor Swift. Até agora temos até o 1988 Taylor's Version. 

* Nova `utilitario_remover_cr()` permite que você remova cabeçalhos e rodapés de arquivos de texto que foram convertidos de PDFs. A função é muito útil quando estamos analisando documentos e precisamos desconsiderar os cabeçalhos e rodapés. Os padrões da função foram testados com acórdãos do Supremo Tribunal Federal e foram suficientes. Talvez para trabalhar com outros tribunais ou documentos seja necessário calibrar a função.
