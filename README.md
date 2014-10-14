russian_stemmer
===============

Russian language Implementation of Porter Stemming Algorithm in Erlang


#In Russian

Предполагается, что на входе функции 
    russian_stemmer:stem(A) 
A - это binary строка в представлении utf8, например: <<"продолжение"/utf8>>
или строка -список в Unicode code point, например: [1087,1088,1086,1076,1086,1083,1078,1077,1085,1080,1077]

Результатом является стемм слова в представлении binary utf8 