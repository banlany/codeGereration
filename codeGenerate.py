from AST_structure import *


class CodeGenerator:
    """
    代码生成类
    """
    targetCode = ''#目标代码
    domain = []#作用域栈
    headFile = []#头文件
    f_stdio = False#头文件标志
    def __init__(self):
        self.targetCode = ''
        self.domain = []
        self.headFile = ''
        self.globalCode = ''
        self.f_stdio = False
        self.ast = Program()
        self.varT = None
        self.mainFucDef:str = 'main'#主函数接口声明 
        self.fucDefList:list[SubFucDef] = []#子函数接口列表 void subFuc1();
        self.fucList:list[SubFuc] = []#子函数定义列表 void subFuc1() {};
        
    
    def genHeadFile(self):
        """
        输出头文件
        """
        if self.f_stdio == True:
            self.headFile+='#include<stdio.h>\n'
        self.headFile+='#include<stdbool.h>\n'
        self.targetCode += self.headFile

    def genSubFucDef(self):
        """
        输出程序接口声明
        """
        pass
    
    def genGlobalDef(self,fuc:SubFuc):
        """
        输出全局变量定义
        """
        for i in len(fuc.constIdList):
            id = fuc.constIdList[i]
            type = fuc.constTypeList[i]
            val = fuc.constValList[i]
            if type == 'int' :
                self.globalCode += 'int ' + id + ' = ' + str(val) + ';\n'
            elif type == 'char' :
                self.globalCode += 'char ' + id + ' = ' + str(val) + ';\n'
            elif type == 'boolen' :
                self.globalCode += 'bool ' + id + ' = ' + str(val) + ';\n'  
            else :
                self.globalCode += 'float ' + id + ' = ' + str(val) + ';\n'
        for i in len(fuc.varIdList):
            id = fuc.varIdList[i]
            type = fuc.varTypeList[i]
            if id in fuc.arr:#为数组
                j = fuc.arr.index(id)
                low = fuc.arrl[j]
                up = fuc.arru[j]
                len1 = up - low + 1
                if type == 'int' :
                    self.globalCode += 'int ' + id + '[' + str(len1) + '];\n'
                elif type == 'char' :
                    self.globalCode += 'char ' + id + '[' + str(len1) + '];\n'
                elif type == 'boolen' :
                    self.globalCode += 'bool ' + id + '[' + str(len1) + '];\n'
                else :
                    self.globalCode += 'float ' + id + '[' + str(len1) + '];\n'
                
            else :
                if type == 'int' :
                    self.globalCode += 'int ' + id + ';\n'
                elif type == 'char' :
                    self.globalCode += 'char ' + id + ';\n'
                elif type == 'boolen' :
                    self.globalCode += 'bool ' + id + ';\n'
                else :
                    self.globalCode += 'float ' + id + ';\n'

    def anaAst(self):
        """
        分析AST
        """
        ast1 = self.ast
        pMainDef = SubFucDef()#void example();
        pMainDef.returnType = 'void'
        pMainDef.id = ast1.programId#主程序名称
        self.fucDefList.append(pMainDef)
        pMain = SubFuc()#main函数
        statement1 = ''
        statement1 += 'void ' + pMainDef.id + '();\n'
        pMain.statementDict[statement1] = 1#完成main函数
        self.fucDefList.append(pMainDef)#添加example定义
        self.fucList.append(pMain)#添加main
        pRealMain = SubFuc()#example实现方法->mainBlock
        realMain = ast1.subProgram #真正的主程序 void example() {}
        #进入主程序，定义的变量为全局变量
        self.genDef(realMain,pRealMain)#定义全局变量
        # for n in realMain.constList['constant'] :
        #     pRealMain.constIdList.append(n.constId)
        #     pRealMain.constTypeList.append(n.type)
        #     pRealMain.constValList.append(n.Value)
        # for d,i in realMain.varList:#程序定义加入常量和变量
        #     if d.type.arrFlag:
        #         pRealMain.arr.append(i)
        #         pRealMain.varTypeList.append(d.type.type)
        #         pRealMain.varIdList.append(i)
        #         pRealMain.arrl.append(d.type.lowerBound)
        #         pRealMain.arru.append(d.type.upperBound)
        #     else:
        #         pRealMain.varIdList.append(i)
        #         pRealMain.varTypeList.append(d.type.type)
        self.genGlobalDef(pRealMain)
        mainBlock = realMain.block#主程序代码块
        #处理主程序代码块
        self.genCompound(mainBlock,pRealMain,1)
        self.fucList.append(pRealMain)
        if realMain.subDefList['type']=='subprogram_declarations':
            fList = realMain.subDefList['subprograms']#子函数定义列表
            for f in fList:
                fuc1 = SubFucDef()#子函数接口声明
                if f.type =='' or f.type =='void':
                    fuc1.returnType = 'void'
                else:
                    fuc1.returnType = f.type
                fuc1.id = f.fucId
                pl = f.formalParaList
                #未处理数组
                for d,l in pl.items():
                    for k,v in d.paraId:
                        if d.flag:#处理传引用
                            pid = '*'+k
                        else:
                            pid =k
                        fuc1.paraList.append(pid)
                        fuc1.paraType.append(d.type)
                        fuc1.paraIsRef.append(d.flag)
                self.fucDefList.append(fuc1)#子函数接口声明完成
                fuc2 = SubFuc()#子函数定义
                fuc2.id = f.fucId
                pblock = f.block
                #处理子程序代码块
                self.genCompound(pblock,fuc2,1)#程序代码块缩进为1
                self.fucList.append(fuc2)#子函数添加完成
    def genExpr(self,expr:Expression):#生成表达式的字符串
        s = ''
        if expr.type == 'var':
            v = expr.varRef
            if v.flag:#如果是数组
                pass#无法获得下标
            else:
                s += v.varId
            return s
        elif expr.type == 'int':
            s += str(expr.value)
            return s
        elif expr.type == 'float':
            s += str(expr.value)
            return s
        elif expr.type == 'function':
            #暂未考虑直接用函数名返回
            f = expr.fucCall
            s += f.fucId+'('
            for p in f.actParaList:
                s += self.genExpr(p)
                if p != f.actParaList[-1]:
                    s += ','
            s += ')'
            return s
        elif expr.type == 'compound':
            if expr.opType == 'double':#双目运算符
                s += '('+self.genExpr(expr.subE1)+expr.operation+self.genExpr(expr.subE2)+')'
            return s
    def genCompound(self,block:Compound,fuc,back):#生成函数的语句字典
        for s in block.statements:
            if s.type == 'compound':
                pass
            else:
                self.genStatement(s,fuc,back)
    def genStatement(self,statemnt:Statement,fuc:SubFuc,back):
        s:Statement = statemnt
        if s.type == 'compound':
            self.genCompound(s.block,fuc,back)
        elif s.type == 'repeat':
            line = ''+'while(!'+self.genExpr(s.condition)+'){'
            fuc.statements.append(line)
            fuc.back.append(back)
            self.genCompound(s.do,fuc,back+1)
            fuc.statements.append('}')
            fuc.back.append(back) 
            """
            while(!xxx == xxx){
                do;
            }
            """
        elif s.type == 'while':
            line = ''+'while('+self.genExpr(s.condition)+'){'
            fuc.statements.append(line)
            fuc.back.append(back)
            self.genCompound(s.do,fuc,back+1)
            fuc.statements.append('}')
            fuc.back.append(back)
            """
            while(xxx == xxx){
                do;
            }
            """
        elif s.type == 'for':
            line = ''+'for('+self.genExpr(s.state)+';'+self.genExpr(s.end)+';'+self.genExpr(s.step)+'){'
            fuc.statements.append(line)
            fuc.back.append(back)
            self.genCompound(s.do,fuc,back+1)
            fuc.statements.append('}')
            fuc.back.append(back) 
            """
            for(xxx;xxx;xxx){
                do;
            }
            """
        elif s.type == 'if':
            line = ''+'if('+self.genExpr(s.condition)+'){'
            fuc.statements.append(line)
            fuc.back.append(back)
            self.genCompound(s.then,fuc,back+1)
            fuc.statements.append('}else{')
            fuc.back.append(back)
            self.genCompound(s.els,fuc,back+1)
            fuc.statements.append('}')
            fuc.back.append(back)
            """
            if(xxx == xxx){
                do;
            }else{
                do;
            }"""
        elif s.type == 'assign':
            v = s.varRef.varId
            line = ''+ v +'='+self.genExpr(s.exp)+';'
            fuc.statements.append(line)
            fuc.back.append(back)
            """
            v = xxx;
            """
        elif s.type =='procall':
            line = ''+s.procId+'('
            for p in s.actParaList:
                line += self.genExpr(p)
                if p != s.actParaList[-1]:
                    line += ','
            line += ');'
            fuc.statements.append(line)
            fuc.back.append(back)
            """
            procId(xxx,xxx,xxx);
            """
            pass
        elif s.type == 'print':
            self.f_stdio = True
            """
            printf("%d",a);
            """
            pass
        elif s.type == 'scan':
            self.f_stdio = True
            """
            scanf("%d",a);
            """
            pass
        pass
    def genDef(self,pfuc:SubProgram,cfuc:SubFuc):#生成函数定义的常量与变量定义
        for n in pfuc.constList['constant'] :
            cfuc.constIdList.append(n.constId)
            cfuc.constTypeList.append(n.type)
            cfuc.constValList.append(n.Value)
        for d,i in pfuc.varList:#程序定义加入常量和变量
            if d.type.arrFlag:
                cfuc.arr.append(i)
                cfuc.varTypeList.append(d.type.type)
                cfuc.varIdList.append(i)
                cfuc.arrl.append(d.type.lowerBound)
                cfuc.arru.append(d.type.upperBound)
            else:
                cfuc.varIdList.append(i)
                cfuc.varTypeList.append(d.type.type)
        

        
                
        

    

class SubFucDef:
    """
    子函数接口声明
    """
    def __init__(self):
        self.returnType:str = ''#"int","float","char","bool" void
        self.id:str = ''#名称
        self.paraList:list[str] =[]#参数
        self.paraIsRef:list[bool] =[]#参数是否引用
        self.paraType:list[str] = [] #参数类型
        self.arr:list = []#是否是数组
        self.arrl:list = []#数组下界
        self.arru:list = []#数组上界
    
    def __str__(self):
        s=''
        s+=self.returnType + ' ' + self.id + '('
        for i in range(len(self.paraList)):
            n = self.paraList[i]
            s+=self.paraType[i]+' '
            s+=n
            if n in self.arr:
                c = self.arr.index(n)
                s+='['+str(self.arru[c]-self.arrl[c]+1)+']'
            if i!=len(self.paraList)-1:
                s+=','
        s+=')'
        return s#不带封号
class SubFuc:
    """
    子函数定义
    """
    def __init__(self):
        self.id:str = ''#名称
        self.constIdList:list[str] = []#常量标识符
        self.constTypeList:list[str] = []#常量类型
        self.constValList:list[str] = []#常量值
        self.varIdList:list[str] = []#变量标识符
        self.varTypeList:list[str] = []#变量类型
        self.arr:list[str] = []#数组 应为变量名
        self.arrl:list = []#数组下界
        self.arru:list = []#数组上界
        self.statements = []#语句
        self.back:list = []#缩进
    
    def outputConstList(self,retract):
        """
        输出常数列表
        """
        pass
    def outputVarList(self,retract):
        """
        输出变量列表
        """
        pass
    
    def outputStatement(self,retract):
        """
        输出语句
        """
        pass
    def outputSubFuc(self,retract):
        """
        输出函数的内容
        """
        pass



        