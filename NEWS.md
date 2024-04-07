# decJ 1.0.0.9001

## Correções

* Os dados que antes eram acessíveis pelo pacote se tornaram internos. Eles ainda podem ser acessados dentro de `R/sysdata.rda`.

* `stf_jurisprudencia_download()` agora funciona como `stf_jurisprudencia()` aceitando tanto a busca por classe quanto por palavras-chave.

* `tjrs_jurisprudencia()` agora possui uma forma de avisar ao usuário que a busca não retornou resultados. 

* As funções agora estão respeitando a regra de caracteres ASCII.

## Novas funções

* Nova `utilitario_tscolor()` permite que você crie uma paleta de cores para gráficos utilizando de cores referente as capas dos albuns da Taylor Swift. Até agora temos até o 1988 Taylor's Version. 

* Nova `utilitario_remover_cr()` permite que você remova cabeçalhos e rodapés de arquivos de texto que foram convertidos de PDFs. A função é muito útil quando estamos analisando documentos e precisamos desconsiderar os cabeçalhos e rodapés. Os padrões da função foram testados com acórdãos do Supremo Tribunal Federal e foram suficientes. Talvez para trabalhar com outros tribunais ou documentos seja necessário calibrar a função.

* Nova `utilitario_remover_acentos()` permite remover acentos do texto. É semelhante a função `txt4cs::remove_accent()` do pacote `txt4cs`, mas não possui dependências.

## Problemas

* As funções de dicionários estavam com problemas devido a sua dependência com o pacote Quanteda. Assim que acharmos uma forma de resolver isso, ela voltará ao pacote. De todo modo, o script da função pode ser encontrado dentro da pasta R/. 

# decJ 1.0.0.9000

## Correções

* Correção da documentação das funções. Algumas funções estavam com exemplo impossível de funcionar pois não respeitavam os parâmetros. Criei também famílias de funções.

## Novas funções

* Nova `alepr_projeto()` permite que você extraia do site da Assembleia Legislativa do Paraná informações sobre proposições legislativas a partir de uma legislação já aprovada.
