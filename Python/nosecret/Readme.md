接口文件脱敏工具使用说明：

# 1. 程序结构说明：
nosecret.py 主程序脚本
input 待脱敏文件存放路径
output 脱敏后文件存放路径
conf 脱敏配置文件存放路径

配置文件说明：
```
[FileNum]
TotalFileNum = 35

[File1]
FileType = TXT
FileInPath = ./input/se001iprdyyyymmdd001.txt
FileOutPath = ./output/se001iprdyyyymmdd001.txt
FileRecSize = 300
FileCharSet = us-ascii
FieldNum = 1
  [Field1]
  FieldStartOffset = 207
  FieldEndOffset = 212
  FieldType = pbu

[File2]
FileType = TXT
FileInPath = ./input/zqdb0yyyymmdd001.txt
FileOutPath = ./output/zqdb0yyyymmdd001.txt
FileRecSize = 11
FileCharSet = us-ascii
FieldNum = 0
......
......
```
以FILE1为样例：
TXT: 文件为文本文件
./input/se001iprdyyyymmdd001.txt: 待脱敏文件绝对路径
./output/se001iprdyyyymmdd001.txt: 脱敏后文件绝对路径
300: 单条文件大小
us-ascii: 文件的编码
1: 待脱敏字段个数

FieldStartOffset: 待脱敏字段开始位置（以1开始）
FieldEndOffset: 待脱敏字段结束位置（以1开始）
pbu: 待脱敏字段为PBU字段，适用PBU脱敏规则

# 2. 脱敏工具使用：
## 2.1 准备
### 2.1.1 待脱敏文件准备：
请按照配置文件中列出的文件准备待脱敏文件，如果文件在配置中不存在，文件不会被脱敏。
如果用户想添加新的脱敏文件，请按照配置文件的格式增加相应配置项（注意：总文件数字段也要相应增加）。
被脱敏文件全部放在input目录下。

### 2.1.2 清空output目录
### 2.1.3 运行工具：
运行前请确保机器已经安装python3.7以上版本：

``` 
python3 nosecret.py -c ./conf/product.ini
```
运行结果样例：
```
--------------------------------------------------------------------------------
process ./input/se001iprd20070313001.txt start...
generate ./output/se001iprd20070313001.txt finish...
--------------------------------------------------------------------------------
Can Not Find A File [./input/zszh030YYYYMMDD001.txt], But It Is In Config File!
Can Not Find A File [./input/se001pbuntbyyyymmdd001.dat], But It Is In Config File!
Can Not Find A File [./input/zhbz1YYYYMMDD001.txt], But It Is In Config File!
Can Not Find A File [./input/se001insassyyyymmdd001.dat], But It Is In Config File!
--------------------------------------------------------------------------------
process ./input/bjye20070313.txt start...
generate ./output/bjye20070313.txt finish...
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
process ./input/se001invntb20070313001.dat start...
generate ./output/se001invntb20070313001.dat finish...
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
process ./input/se001pbsj20070313001.txt start...
generate ./output/se001pbsj20070313001.txt finish...
--------------------------------------------------------------------------------
Can Not Find A File [./input/se063bcanacctyyyymmdd001.txt], But It Is In Config File!
Can Not Find A File [./input/se001intsutyyyymmdd001.dat], But It Is In Config File!
Can Not Find A File [./input/se012flreleaseYYYYMMDD001.txt], But It Is In Config File!
Can Not Find A File [./input/se053zspjYYYYMMDD001.txt], But It Is In Config File!
--------------------------------------------------------------------------------
process ./input/se012hypb20070313001.txt start...
generate ./output/se012hypb20070313001.txt finish...
--------------------------------------------------------------------------------
Can Not Find A File [./input/se001ntsparyyyymmdd001.dat], But It Is In Config File!
-------------------------------
```
Can Not Find A File: 表示配置中包含这个文件，但是用户却没有准备这个文件；

# 3. 注意事项：
+ 使用之前一定要确定 所准备的文本文件的字符编码 和 配置文件中的字符编码一致；
  Linix/Unix 上用 file -i filepath查看
  Mac        上用 file -I filepath查看
+ AZDJ 这样的大文件转换时间较长，请耐心等待；
