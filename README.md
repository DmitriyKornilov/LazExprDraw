Модули **LazExprDraw** и **LazExprMake** являются кросплатформенным (Windows+Linux) вариантом для Lazarus  модулей ExprDraw и ExprMake для отрисовки математических формул на TCanvas, написанных Антоном Григорьевым на Delphi 5 и опубликованных в 2002 году.

В оригинале использовались функции GDI/Windows API и проприетарный шрифт Times New Roman, что исключало/затрудняло использование кода в Linux. Здесь же используются стандартные возможности TCanvas и шрифта XITS, распространяемого по свободной лицензии [SIL Open Font License](https://opensource.org/licenses/OFL-1.1). 

Шрифты XITS доступны на [github](https://github.com/alif-type/xits). Если нужны только стандартные функции модулей и не требуется особого форматирования текста, то необходимы только два файла с начертаниями Regular и Italic.

*LazExprDraw содержит классы, использующиеся для отображения математических формул.
*LazExprMake используется для создания классов на основании символьной записи формулы.
*Руководство пользователя приведено в "LazExprDraw_Guide.pdf".
*Приложение LazExprGuide - это справочник по использованию LazExprMake и демо

Учитывая пожелания автора оригинального кода, модули LazExprDraw и LazExprMake предлагаются по свободной лицензии [MIT](https://opensource.org/licenses/MIT) 

___

Units **LazExprDraw** and **LazExprMake** are cross platform (Windows+Linux) versions for the Lazarus of units ExprDraw and ExprMake for drawing mathematical formulas on TCanvas, written by Anton Grigoriev in Delphi 5 and published in 2002.

In the original, GDI / Windows API functions and the proprietary font Times New Roman were used, which excluded / made it difficult to use the code in Linux. It also uses the standard features of TCanvas and the XITS font, distributed under a free license [SIL Open Font License](https://opensource.org/licenses/OFL-1.1).

XITS fonts are available at [github](https://github.com/alif-type/xits). If only standard functions of units are needed and no special text formatting is required, then only two files with Regular and Italic styles are needed.

*LazExprDraw unit contains classes used to display mathematical formulas.
*LazExprMake unit is used to create classes based on a symbolic formula entry.
*The user manual is provided in "LazExprDraw_Guide.pdf".
*The LazExprGuide application is a guide to using symbolic formula entry in unit LazExprMake and the demo.

Considering the wishes of the author of the original code, the LazExprDraw and LazExprMake modules are offered under the free license [MIT](https://opensource.org/licenses/MIT)
