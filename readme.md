# KBR相位中心标定程序文档

## 一、依赖环境

1. windows：vs2019+intel oneapi 2021
2. linux：intel oneapi 2021(采用默认安装路径)+python package(ford, FoBiS.py)

## 二、xml文件

xml文件中**product**项不可随意更改，且应当在“category”中标记清楚输入、输出。

| product |                       代表数据                       |
| :-----: | :--------------------------------------------------: |
| GNI1B_C |               主星1B级惯性系位置、速度               |
| GNI1B_D |               从星1B级惯性系位置、速度               |
|  KBR1B  | 双星1B级微波测距数据，需含有带偏星间距和飞行时间改正 |
| SCA1B_C |        主星1B级惯性系与科学参考框架转换四元数        |
| SCA1B_D |        从星1B级惯性系与科学参考框架转换四元数        |

