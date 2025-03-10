* Encoding: UTF-8.
--------------------RECODIFICACIONES PARA INVERTIR LIKERT

RECODE P4_1 (1=4) (2=3) (3=2) (4=1) (MISSING=SYSMIS) INTO urgenciacrisis. 
VARIABLE LABELS  urgenciacrisis 'P4_1b'. 
EXECUTE. 
RECODE P4_3 (1=4) (2=3) (3=2) (4=1) (MISSING=SYSMIS) INTO actividadeshumanas. 
VARIABLE LABELS  actividadeshumanas 'P4_3b'. 
EXECUTE. 

----------------------ESCALAS MODO DE VIDA IMPERIAL

COMPUTE escala_mvi=P7_2 + P7_4+ P7_5 + P4_2 + P4_4. 
VARIABLE LABELS  escala_mvi 'Escala Modo de Vida Imperial'. 
EXECUTE.

P7_2 [Los problemas sociales, como la equidad, salud, etc., son más importantes que los problemas ambientales] 
P7_4 [Se puede crecer económicamente sin dañar el medioambiente]
P7_5 [Los problemas ambientales son preocupaciones de los países desarrollados que tienen su situación socio-económica resuelta]
P4_2 [No es necesario cambiar nuestras costumbres, los problemas ambientales pueden resolverse con tecnología]
P4_4 [El progreso para Chile significa que la mayoría de nosotros podamos vivir como en los países desarrollados]

FACTORIAL 2

+++[No es necesario cambiar nuestras costumbres, los problemas ambientales pueden resolverse con tecnología] Ahora quiero leer algunas afirmaciones y que usted me Indique qué tan de acuerdo o en desacuerdo está usted con ellas, en una escala de 1 a 4, en d
+++[Los problemas sociales, como la equidad, salud, etc., son más importantes que los problemas ambientales] Respecto a las siguientes frases y en una escala de 1 a 4, en donde 1 es muy en desacuerdo y 4 es muy de acuerdo ¿Qué tan de acuerdo está usted co
[Los modos de vida de las clases sociales más altas son más responsable de la crisis ambiental] Respecto a las siguientes frases y en una escala de 1 a 4, en donde 1 es muy en desacuerdo y 4 es muy de acuerdo ¿Qué tan de acuerdo está usted con las sig
+++[Los problemas ambientales son preocupaciones de los países desarrollados que tienen su situación socio-económica resuelta] Respecto a las siguientes frases y en una escala de 1 a 4, en donde 1 es muy en desacuerdo y 4 es muy de acuerdo ¿Qué tan de ac

COMPUTE escala_mvi_II=P7_2 + P7_3+ P7_5 + P4_2 + P4_4. 
VARIABLE LABELS  escala_mvi_II 'Escala Modo de Vida Imperial II'. 
EXECUTE.

COMPUTE escala_mvi_III=P7_2 + P7_3+ P7_5 + P4_2. 
VARIABLE LABELS  escala_mvi_III 'Escala Modo de Vida Imperial III'. 
EXECUTE.

----------------------ESCALAS REFLEXIVIDAD

COMPUTE escala_raa=P7_1 + P7_3 + P4_1 + P4_3. 
VARIABLE LABELS  escala_raa 'Escala Reflexividad Ambiental Antropocentrica'. 
EXECUTE.

P7_1 [Los problemas ambientales pueden resolverse cambiando nuestras costumbres]
P7_3 [Los modos de vida de las clases sociales más altas son más responsable de la crisis ambiental] 
P4_1 [La crisis ambiental es un problema que hay que enfrentar con urgencia]
P4_3 [Las actividades humanas son la principal causa de los cambios climáticos]

FACTORIAL1

+++[La crisis ambiental es un problema que hay que enfrentar con urgencia] Ahora quiero leer algunas afirmaciones y que usted me Indique qué tan de acuerdo o en desacuerdo está usted con ellas, en una escala de 1 a 4, en donde 1 es muy en desacuerdo y 4 es
+++[Las actividades humanas son la principal causa de los cambios climáticos] Ahora quiero leer algunas afirmaciones y que usted me Indique qué tan de acuerdo o en desacuerdo está usted con ellas, en una escala de 1 a 4, en donde 1 es muy en desacuerdo y 4
+++[Los problemas ambientales pueden resolverse cambiando nuestras costumbres] Respecto a las siguientes frases y en una escala de 1 a 4, en donde 1 es muy en desacuerdo y 4 es muy de acuerdo ¿Qué tan de acuerdo está usted con las siguientes afirmaciones?
[Se puede crecer económicamente sin dañar el medioambiente] Respecto a las siguientes frases y en una escala de 1 a 4, en donde 1 es muy en desacuerdo y 4 es muy de acuerdo ¿Qué tan de acuerdo está usted con las siguientes afirmaciones?


----------------------ÍNDICE AKATU

COMPUTE indice_akatu=O1_1 + O1_4 + O1_2 + O1_5 + O1_6 + O1_7 + O1_8 + O1_9 + O1_10 + O1_11 + O1_12 + O1_13 + O1_14 + O1_15.
VARIABLE LABELS  indice_akatu 'Indice de Consumo Sustentable AKATU'. 
EXECUTE.

-----------------CONSUMER CULTURE

COMPUTE consumer_culture=C1_1 + C1_3 + C1_4 + C1_2 + C1_5 + C1_6 + C1_7.
VARIABLE LABELS  consumer_culture 'Índice de Cultura de Consumo'. 
EXECUTE.


-----------------------ESCALAS PROMEDIO

COMPUTE escala_mvi_mean=MEAN(P7_2, P7_5, P7_4, P4_2, P4_4). 
VARIABLE LABELS  escala_mvi_mean 'Escala Modo de Vida Imperial M'. 
EXECUTE.

COMPUTE escala_raa_mean=MEAN(P7_1, P7_3, P4_1, P4_3). 
VARIABLE LABELS  escala_raa_mean 'Escala Escala Reflexividad Ambiental Antropocentrica M'. 
EXECUTE.

COMPUTE escala_akatu_mean=MEAN(O1_1, O1_4, O1_2, O1_5, O1_6, O1_7, O1_8, O1_9, O1_10, O1_11, O1_12, O1_13, O1_14, O1_15). 
VARIABLE LABELS  escala_akatu_mean 'Escala AKATU M'. 
EXECUTE.

COMPUTE consumer_culture_mean=MEAN(C1_1, C1_3, C1_4, C1_2, C1_5, C1_6, C1_7). 
VARIABLE LABELS  consumer_culture_mean 'Escala Consumo M'. 
EXECUTE.
----------------

Consumo: Máximo=muy consumer culture 7-28
AKATU: Máximo=poco sustentable 14-56
Modo de Vida Imperial: Máximo=adhiere MVI 5-20
Reflexividad: Máximo=muy crítico 4-16

Deshacer la escala la de reflexividad
Trasladar la pregunta de clases sociales a la escala de modo de vida imperial
Quitar progreso de la escala


