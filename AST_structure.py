
#符号表格式
'''
p={'const':[{'id':'',
            'type':'',
            'value':},]
'var':[{'id':'',
            'type':'',
            'value':}
        ,]
        
table=[{'funcid':{
        'type':''
        'const':[{'id':'',
            'type':'',
            'value':},] ,
        'var':[{'id':'',
            'type':'',
            'value':},] ,
        'subF':[str,]
        },]
'''
SymbolTable=[]
    


#语法树 

class Program:
    """
    主程序类
    """
    def __init__(self,a,b,c,d):
        self.programId:str = a#程序标题声明名
        self.line:int = b      #程序标题行号
        self.paraDict:dict = c #参数标识符:行号
        self.subProgram:SubProgram =d

class SubProgram:
    """
    子程序类
    """
    def __init__(self,a,b,c,d):
        self.constList: dict = a if a else {}  # 常量定义列表
        self.varList: dict = b if b else {}    # 变量定义列表
        self.subDefList: dict = c if c else {}  # 子程序和子函数
        self.block: Compound = Compound(d)  # 主程序体
class Constant:
    """
    常量定义类
    """
    def __init__(self,a,b,c,d):
        self.constId:str = a  #标识符
        self.line:int = b       #行号
        self.type:str = c      #类型int,char,real,string（这个string指的是常量标识符的ID，因为可能由其他量赋值）
        self.Value= d           #值


class Variant:
    """
    变量定义类
    """
    def __init__(self, idlist,type):
        self.idlist: dict = idlist  # 标识符名称
        self.type: Type = type  # 变量类型


class Type:
    """
    变量类型类
    """
    def __init__(self,a='',b=0,c=False,d=0,e=0):
        self.type:str = a       #基本类型，int、char、real、boolean
        self.line:int = b       #行号
        self.arrFlag:bool= c    #0不是数组，1是数组
        self.lowerBound:list[int] = d  #数组下界（n维）
        self.upperBound:list[int] = e  #数组上界

class FucDefn:
    """
    过程与函数定义类
    """
    def __init__(self,name,lineno,formal,types,con,var,block):
        self.funcId:str = name      #名称
        self.line:int = lineno        #行号
        self.formalParaList:dict = formal if formal else {}#形参字典 数字：FormalParameter
        self.type:str = types       #如果type是空串，则为过程，否则为函数,取值为"integer","real","boolean","char"四种
        self.constList:dict = con if con else {} #常数定义列表
        self.varList:dict = var if var else {} #变量定义列表
        self.block:Compound = Compound(block) #程序体


class FormalParameter:
    """
    形参类
    """
    def __init__(self):
        self.paraId:list  #标识符
        self.line:int    #行号
        self.type:str  #基本类型，int、char、real、boolean
        self.flag:bool = False#false代表传值调用，true代表引用调用

class Expression:
    """
    表达式类
    """
    def __init__(self,ty,line,value=''):
        self.type:str = ty
        """
        表达式类型,"var"表示变量,"int"表示整数,"float"表示浮点数,
        "function"表示函数调用,
        "compound"表示复合表达式,compound有普通的二目运算符
        还有minus、not、bracket等单目运算符 
        """
        self.line=line
        self.varRef:VarReference =None
        self.value=value
        self.fucCall:FucCall =None
        self.operation:str = ''#复合表达式
        self.opType:str = ''#操作符类型单/双
        self.subE1:Expression=None #子式1
        self.subE2:Expression =None#子式2

class FucCall:
    """
    函数调用
    """
    def __init__(self,id,line,exp):
        self.fucId:str = id#函数名称
        self.line:int = line
        self.actParaList:list[Expression]=exp #参数列表

class VarReference:
    """
    变量引用
    """
    def __init__(self,id,line,exp,flag=0):
        self.varId:str = id#变量名
        self.line:int = line
        self.expList:list[Expression] =exp
        self.flag:bool = flag #0表示非数组,1表示数组

class Statement:
    """
    语句类
    """
    def __init__(self):
        self.line:int = 0#行号
        self.type:str = ''#类型"compound","repeat","while","for","if","assign","procall","print","scan"
        self.stateType:str = ''#区别于type，取值为"void"或"error"
class Assign(Statement):
    """
    赋值
    """
    def __init__(self):
        super().__init__()
        self.varRef:VarReference #左值变量
        self.exp:Expression #右边表达式
class If(Statement):
    """
    条件语句
    """
    def __init__(self):
        super().__init__()
        self.condition:Expression #条件表达式
        self.then:Statement #then语句
        self.els:Statement #else语句
class While(Statement):
    """
    循环语句
    """
    def __init__(self):
        super().__init__()
        self.condition:Expression #条件表达式
        self.do:Statement #循环体
class For(Statement):
    """
    for循环语句
    """
    def __init__(self):
        super().__init__()
        self.id:str = ''#循环变量
        self.state:Expression #起始值
        self.end:Expression #结束值
        self.do:Statement #执行语句
class Repeat(Statement):
    """
    repeat语句
    """
    def __init__(self):
        super().__init__()
        self.condition:Expression = Expression()#条件
        self.do:Statement = Statement()

class Compound(Statement):
    """
    复合语句类
    """
    def __init__(self,block=[]):
        super().__init__()
        self.statements:list[Statement] = block#语句列表

class ProcCall(Statement):
    """
    过程调用
    """
    def __init__(self):
        super().__init__()
        self.procId:str = ''#名称
        self.actParaList:list[Expression] = []#实参

class Print(Statement):
    '''
    标准输入，printf'''
    def __init__(self):
        super().__init__()
        self.varlist:list   #参数列表

class Scan(Statement):
    '''
    标准输出，scanf'''
    def __init__(self):
        super().__init__()
        self.varlist:list #参数列表




# 在主程序中创建 ProgramPrinter 实例并调用打印方法以打印 Program 实例的详细内容
if __name__ == '__main__':
    # printer = ProgramPrinter()
    # 假设有一个名为 my_program 的 Program 实例
    my_program = Program("program1", 10, {"param1": 20, "param2": 30}, SubProgram({}, {}, {}, []))
    # printer.print_program_details(my_program)
