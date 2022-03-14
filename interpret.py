import ply.yacc as yacc
import ply.lex as lex
import grammar
import parser
import sys

def lookup(key,env):
	if env['env'].get(key) is not None:
		return env['env'][key]
	else:
		if env['parent'] is not None:
			return lookup(key,environments[env['parent']])
		else:
			return None

def assign(key,value,env):
	if env['env'].get(key) is not None:
		#print env['env'][key]['type'], value[0] 
		if env['env'][key]['type'] == value[0]:
			env['env'][key]['value'] = value
		else:
			print ("Type Error")
			exit()
	else:
		if env['parent'] is not None:
			assign(key,value,environments[env['parent']])
		else:
			print ("No such variable")
			exit()

def arrayAssign(key,index,value,env):
	if env['env'].get(key) is not None:
		if env['env'][key]['isArray']==1 and env['env'][key]['type'] == value[0] and index[1] >= 0 and index[1] < env['env'][key]['arrayLength']:
			env['env'][key]['value'][index[1]] = value
		else:
			print ("Type Error")
			exit()
	else:
		if env['parent'] is not None:
			arrayAssign(key,index,value,environments[env['parent']])
		else:
			print ("No such variable")
			exit()

def interpret(tree):
	if tree[0] == "maincall":
		environments['main'] = {'env': {}, 'parent': None, 'name':'main'}
		for stmt in tree[1]:
			evaluatestmt(stmt,environments['main'])
	if tree[0] == "function":
		if functionTrees.get(tree[2]) is None:
			functionTrees[tree[2]] = {'args': tree[3], 'returnType': tree[1], 'name': tree[2], 'stmts': tree[4]}
	if tree[0] == "classdefinition":
		if classes.get(tree[1]) is None:
			classes[tree[1]] = {'name':tree[1], 'attributes':[], 'functions':{}}
			array = tree[2]
			for feature in tree[2]:
				if feature[0] == "classattribute":
					classes[tree[1]]['attributes'].append((feature[1],feature[2]))
				elif feature[0] == "classfunction":
					classes[tree[1]]['functions'][feature[2]] = {'args': feature[3], 'returnType': feature[1], 'name': feature[2], 'stmts': feature[4]}
			#print classes 
def evaluateexp(exp,env): #Must return a (type,value) pair always!!!!
	if exp[0] == "negative":
		exp1 = evaluateexp(exp[1],env)
		#print exp1
		if exp1[0]=="int" or exp1[0]=="double":
			val = 0 - exp1[1]
			res = (exp1[0],val)
			#print res
			return res
		else:
			print ("Type Error")
			exit()
	if exp[0] == "BinaryOperation":
		#print exp
		operator = exp[2][0]
		if operator == "plus":
			exp1 = evaluateexp(exp[1],env)
			exp2 = evaluateexp(exp[3],env)
			if (exp1[0] == "int" and exp2[0] == "int"):
				val = int(exp1[1] + exp2[1])
				return ("int",val)
			elif (exp1[0] == "double" and exp2[0] == "double"):
				val = exp1[1] + exp2[1]
				return ("double",val)
			elif (exp1[0] == "string" and exp2[0] == "string"):
				val = exp1[1] + exp2[1]
				return ("string",val)
			else:
				print ("Data types are not compatible")
				exit()
		elif operator == "minus":
			if exp[1] == [] and (exp[3][0] == "int" or exp[3][0] == "double"):
				etype = exp[3][0]
				exp = (exp[0],(etype,0),exp[2],exp[3])
			exp1 = evaluateexp(exp[1],env)
			exp2 = evaluateexp(exp[3],env)
			if (exp1[0] == "int" and exp2[0] == "int"):
				val = int(exp1[1] - exp2[1])
				return ("int",val)
			elif (exp1[0] == "double" and exp2[0] == "double"):
				val = exp1[1] - exp2[1]
				return ("double",val)
			else:
				print ("Data types are not compatible")
				exit()
		elif operator == "mult":
			exp1 = evaluateexp(exp[1],env)
			exp2 = evaluateexp(exp[3],env)
			if (exp1[0] == "int" and exp2[0] == "int"):
				val = int(exp1[1] * exp2[1])
				return ("int",val)
			elif (exp1[0] == "double" and exp2[0] == "double"):
				val = exp1[1] * exp2[1]
				return ("double",val)
			else:
				print ("Data types are not compatible")
				exit()
		elif operator == "divide":
			exp1 = evaluateexp(exp[1],env)
			exp2 = evaluateexp(exp[3],env)

			if (exp1[0] == "int" and exp2[0] == "int"):
				val = int(exp1[1] / exp2[1])
				return ("int",val)
			elif (exp1[0] == "double" and exp2[0] == "double"):
				val = exp1[1] / exp2[1]
				return ("double",val)
			else:
				print ("Data types are not compatible")
				exit()
		elif operator == "mod":
			exp1 = evaluateexp(exp[1],env)
			exp2 = evaluateexp(exp[3],env)

			if (exp1[0] == "int" and exp2[0] == "int"):
				val = int(exp1[1] % exp2[1])
				return ("int",val)
			else:
				print ("Data types are not compatible")
				exit()
		elif operator == "gt":
			exp1 = evaluateexp(exp[1],env)
			exp2 = evaluateexp(exp[3],env)

			if (exp1[0] == "string" and exp2[0] == "string") or (exp1[0] != "string" and exp2[0] != "string"):
				val = int(exp1[1] > exp2[1])
				return ("int",val)
			else:
				print ("Data types are not compatible")
				exit()
		elif operator == "ge":
			exp1 = evaluateexp(exp[1],env)
			exp2 = evaluateexp(exp[3],env)

			if (exp1[0] == "string" and exp2[0] == "string") or (exp1[0] != "string" and exp2[0] != "string"):
				val = int(exp1[1] >= exp2[1])
				return ("int",val)
			else:
				print ("Data types are not compatible")
				exit()
		elif operator == "lt":
			exp1 = evaluateexp(exp[1],env)
			exp2 = evaluateexp(exp[3],env)

			if (exp1[0] == "string" and exp2[0] == "string") or (exp1[0] != "string" and exp2[0] != "string"):
				#print exp1[1],"   ",exp2[1]
				val = int(exp1[1] < exp2[1])
				#print 'val = ',val
				return ("int",val)
			else:
				print ("Data types are not compatible")
				exit()
		elif operator == "le":
			exp1 = evaluateexp(exp[1],env)
			exp2 = evaluateexp(exp[3],env)

			if (exp1[0] == "string" and exp2[0] == "string") or (exp1[0] != "string" and exp2[0] != "string"):
				val = int(exp1[1] <= exp2[1])
				return ("int",val)
			else:
				print( "Data types are not compatible")
				exit()
		elif operator == "eqeq":
			exp1 = evaluateexp(exp[1],env)
			exp2 = evaluateexp(exp[3],env)

			if (exp1[0] == "string" and exp2[0] == "string") or (exp1[0] != "string" and exp2[0] != "string"):
				val = int(exp1[1] == exp2[1])
				return ("int",val)
			else:
				print ("Data types are not compatible")
				exit()
		elif operator == "ne":
			exp1 = evaluateexp(exp[1],env)
			exp2 = evaluateexp(exp[3],env)

			if (exp1[0] == "string" and exp2[0] == "string") or (exp1[0] != "string" and exp2[0] != "string"):
				val = int(exp1[1] != exp2[1])
				return ("int",val)
			else:
				print ("Data types are not compatible")
				exit()
	elif exp[0] == "UnaryOperationL":
		if exp[1][0] == 'plusplus':
			element = evaluateexp(exp[2],env)
			#print 'E: ',element
			if element[0] == "int" or element[0]=="double":
				val = element[1] + 1
				result = (element[0],val)
				if exp[2][0] == "variable":
					identifier = exp[2][1]
					assign(identifier,result,env)
				elif exp[2][0]=="ArrayElement":
					index = evaluateexp(exp[2][2],env)
					arrayAssign(exp[2][1],index,result,env)
				return result
			else:
				print ("Data Types not compatible")
				exit()
				return None
		elif exp[1][0] == "minusminus":
			element = evaluateexp(exp[2],env)
			#print 'E: ',element
			if element[0] == "int" or element[0]=="double":
				val = element[1] - 1
				result = (element[0],val)
				if exp[2][0] == "variable":
					identifier = exp[2][1]
					assign(identifier,result,env)
				elif exp[2][0]=="ArrayElement":
					index = evaluateexp(exp[2][2],env)
					arrayAssign(exp[2][1],index,result,env)
				return result
			else:
				print ("Data Types not compatible")
				exit()
				return None
	elif exp[0] == "UnaryOperationR":
		if exp[2][0] == 'plusplus':
			element = evaluateexp(exp[1],env)
			#print 'E: ',element
			if element[0] == "int" or element[0]=="double":
				val = element[1] + 1
				result = (element[0],val)
				if exp[1][0] == "variable":
					identifier = exp[1][1]
					assign(identifier,result,env)
				elif exp[1][0]=="ArrayElement":
					index = evaluateexp(exp[1][2],env)
					arrayAssign(exp[1][1],index,result,env)
				return result
			else:
				print ("Data Types not compatible")
				exit()
				return None
		elif exp[2][0] == "minusminus":
			element = evaluateexp(exp[1],env)
			#print 'E: ',element
			if element[0] == "int" or element[0]=="double":
				val = element[1] - 1
				result = (element[0],val)
				if exp[1][0] == "variable":
					identifier = exp[1][1]
					assign(identifier,result,env)
				elif exp[1][0]=="ArrayElement":
					index = evaluateexp(exp[1][2],env)
					arrayAssign(exp[1][1],index,result,env)
				return result
			else:
				print ("Data Types not compatible")
				exit()
				return None

	elif exp[0] in ['int','string','double','char','bool']:
		#print 'I come here'
		return exp
	elif exp[0] == "variable":
		tmp = lookup(exp[1],env)
		if tmp is not None:
			return tmp['value']
		else:
			print ("No such variable")
			exit()
			return None
	elif exp[0] == "ArrayElement":
		index = evaluateexp(exp[2],env)
		if index[0]=="int" and index[1] >= 0:
			index = index[1]
			#print index
			tmp = lookup(exp[1],env)
			#print 'tmp: ',tmp
			if tmp is not None:
				if tmp['isArray']==1 and index < tmp['arrayLength']:
					return tmp['value'][index]
				else:
					print ("Invalid index")
					exit()
					return None
			else:
				print ("No such array")
				exit()
				return None
		else:
			print ("Invalid index")
			exit()
			return None
	elif exp[0] == "Paranthesis":
		return evaluateexp(exp[1],env)
	elif exp[0] == "functioncall":
		name = exp[1]
		if fCallCounts.get(name) is None:
			fCallCounts[name] = 1
		else:
			fCallCounts[name] +=1
		if functionTrees.get(name) is not None:
			envName = name + str(fCallCounts[name])
			environments[envName] = {'name':envName, 'parent':None, 'env':{}}
			args = exp[2] #this is an array
			argsNeeded = functionTrees[name]['args']
			if len(args) == len(argsNeeded):
				for i,a in enumerate(args):
					tmp = evaluateexp(a,env)
					#print tmp
					#print argsNeeded[i]
					if tmp[0] == argsNeeded[i][0][1]:
						environments[envName]['env'][argsNeeded[i][1]] = {'isArray':0, 'arrayLength':-1, 'type':tmp[0], 'value':tmp}
						#print 'New Env:',environments[envName]['env']
					else:
						print ('Data Types do not match!')
						exit()
						return None
				environments[envName]['env']['return'] = {'isArray':0, 'arrayLength':-1, 'type':functionTrees[name]['returnType'][1], 'value':(functionTrees[name]['returnType'][1],None)}
				for st in functionTrees[name]['stmts']:
					evaluatestmt(st,environments[envName])
					if st[0] == "Return":
						break

				if environments[envName]['env'].get('return') is not None:
						if functionTrees[name]['returnType'][1] == environments[envName]['env']['return']['value'][0]:
							return environments[envName]['env']['return']['value']
						else:
							print ("Return type error!")
							exit()
							return None
				else:
					return None

			else:
				print ("Not the correct number of arguments!!")
				exit()
	elif exp[0] == "ObjectAttribute":
		return objects[exp[1]]['attributes'][exp[2]]['value']
	elif exp[0] == "objectFunctionCall":
		#print 'Yo'
		objName = exp[1]
		if objects.get(objName) is not None:
			#print 'Hi'
			className = objects[objName]['className']
			args = exp[3]
			fName = exp[2]
			if classes[className]['functions'].get(fName) is not None:
				#print 'Hello'
				argsNeeded = classes[className]['functions'][fName]['args']
				if len(argsNeeded) == len(args):
					fEnv = {'parent':None, 'name':objName + fName, 'env':{}}
					for i,a in enumerate(args):
						tmp = evaluateexp(a,env)
						if tmp[0] == argsNeeded[i][0][1]:
							fEnv['env'][argsNeeded[i][1]] = {'isArray':0,'arrayLength':-1,'type':tmp[0],'value':tmp}
						else:
							print ("Data Types do not match!")
							exit()
							return None
					for key in objects[objName]['attributes']:
						fEnv['env'][key] = {'isArray':0,'arrayLength':-1, 'type':objects[objName]['attributes'][key]['type'], 'value':objects[objName]['attributes'][key]['value']}

					#print fEnv,'\n','\n'
					fEnv['env']['return'] = {'isArray':0, 'arrayLength':-1, 'type':classes[className]['functions'][fName]['returnType'][1], 'value':(classes[className]['functions'][fName]['returnType'][1],None)}
					#print fEnv,'\n','\n','\n'
					for st in classes[className]['functions'][fName]['stmts']:
						evaluatestmt(st,fEnv)
						if st[0]=="Return":
							break
					return fEnv['env']['return']['value']




def evaluatestmt(stmt,env):
	if len(stmt)==0:
		return

	if stmt[0] in expression:
		evaluateexp(stmt,env)

	elif stmt[0] == "declare-simple-variable":

		env['env'][stmt[2]] = {'type':stmt[1][1], 'value':None, 'isArray':0, 'arrayLength':-1} #-------------------------------------------

	elif stmt[0] == "initialize-simple-variable":
		if stmt[2] in env['env'].keys():
			print("Redeclaration of existing variable is not allowed")
			exit()
			
		exp = evaluateexp(stmt[3],env) #should return (type,value) pair
		print (stmt)
		#print '...',exp
		#print exp
		if exp is not None:
			#print exp, '!!'
			if stmt[1][1] == exp[0]:
				env['env'][stmt[2]] = {'type':stmt[1][1], 'value':exp, 'isArray':0, 'arrayLength':-1} #------------------------------------
				#evaluatestmt(stmt[4],env)
			else:
				print ("Type Error")
				exit()
	elif stmt[0] == "MultipleVariables":
		theVars = stmt[2]
		for variable in theVars:
			env['env'][variable] = {'type':stmt[1][1], 'value':None, 'isArray':0, 'arrayLength':-1} #---------------------------------------
	elif stmt[0] == "SimpleVarAssign":
		exp = evaluateexp(stmt[2],env)
		if exp is not None:
			assign(stmt[1],exp,env)
	elif stmt[0] == "arraydeclaration":
		env['env'][stmt[2]] = {'isArray':1,'arrayLength':stmt[3],'type':stmt[1][1],'value':[(stmt[1][1],None)]*stmt[3]} #---------------------
	elif stmt[0] == "ArrayInitialisation":
		theArray = stmt[3]
		correct = 1
		for ele in theArray:
			if ele[0] != stmt[1][1]:
				print ('Element type does not match array type')
				correct = 0
				exit()
		if correct==1:
			env['env'][stmt[2]] = {'isArray':1,'arrayLength':len(theArray),'type':stmt[1][1],'value':theArray} #-------------------------------	
	elif stmt[0] == "ArrayInitialisationWithLength":
		theArray = stmt[4]
		correct = 1
		if len(theArray)!=stmt[3]:
			correct = 0
		for ele in theArray:
			if ele[0] != stmt[1][1]:
				print ('Element type does not match array type')
				correct = 0
				exit()
		if correct==1:
			env['env'][stmt[2]] = {'isArray':1,'arrayLength':len(theArray),'type':stmt[1][1],'value':theArray} #---------------------------------		

	elif stmt[0] == "ArrayIndexAssignment":
		expI = evaluateexp(stmt[2],env)
		expV = evaluateexp(stmt[3],env)
		if expI is not None:
			arrayAssign(stmt[1],expI,expV,env)
	elif stmt[0] == "if-then":
		exp = evaluateexp(stmt[1],env)
		if exp[1] != 0:
			newName = env['name'] + '-child'
			environments[newName] = {'parent':env['name'], 'env': {}, 'name':newName}
			for s in stmt[2]:
				#print environments['main'],'\n'
				evaluatestmt(s,environments[newName])
				#print environments['main'],'\n'
			environments.pop(newName,None)
	elif stmt[0] == "if-then-else":
		exp = evaluateexp(stmt[1],env)
		if exp[1] != 0:
			newName = env['name'] + '-child'
			environments[newName] = {'parent':env['name'], 'env': {}, 'name':newName}
			for s in stmt[2]:
				#print environments['main'],'\n'
				evaluatestmt(s,environments[newName])
				#print environments['main'],'\n'
			environments.pop(newName,None)
		else:
			newName = env['name'] + '-child'
			environments[newName] = {'parent':env['name'], 'env': {}, 'name':newName}
			for s in stmt[3]:
				#print environments['main'],'\n'
				evaluatestmt(s,environments[newName])
				#print environments['main'],'\n'
			environments.pop(newName,None)
	elif stmt[0] == "if-else-if":
		exp = evaluateexp(stmt[1],env)
		if exp[1] != 0:
			newName = env['name'] + '-child'
			environments[newName] = {'parent':env['name'], 'env': {}, 'name':newName}
			for s in stmt[2]:
				#print environments['main'],'\n'
				evaluatestmt(s,environments[newName])
				#print environments['main'],'\n'
			environments.pop(newName,None)
		else:
			evaluatestmt(stmt[3],env)
	elif stmt[0] == "else-if":
		exp = evaluateexp(stmt[1],env)
		if exp[1] != 0:
			newName = env['name'] + '-child'
			environments[newName] = {'parent':env['name'], 'env': {}, 'name':newName}
			for s in stmt[2]:
				#print environments['main'],'\n'
				evaluatestmt(s,environments[newName])
				#print environments['main'],'\n'
			environments.pop(newName,None)
		else:
			evaluatestmt(stmt[3],env)
	elif stmt[0] == "else":
		newName = env['name'] + '-child'
		environments[newName] = {'parent':env['name'], 'env': {}, 'name':newName}
		for s in stmt[1]:
			#print environments['main'],'\n'
			evaluatestmt(s,environments[newName])
			#print environments['main'],'\n'
		environments.pop(newName,None)
	elif stmt[0] == "For Loop":
		newName = env['name'] + '-child'
		environments[newName] = {'parent':env['name'], 'env': {}, 'name':newName}
		evaluatestmt(stmt[1],environments[newName])
		terminatingCond = evaluateexp(stmt[2],environments[newName])
		#print 'terminating condition: ',terminatingCond
		while (terminatingCond[1] != 0):
			for s in stmt[4]:
				evaluatestmt(s,environments[newName])
			evaluatestmt(stmt[3],environments[newName])
			terminatingCond = evaluateexp(stmt[2],environments[newName])
			#print 'terminating condition: ',terminatingCond
		environments.pop(newName,None)
	elif stmt[0] == "Return":
		exp = evaluateexp(stmt[1],env)
		#print 'exp:::::' ,exp
		if exp is not None:
			assign('return',exp,env)
	elif stmt[0] == "print":
		exp = evaluateexp(stmt[1],env)
		print (exp[1])

	elif stmt[0] == "printn":

		exp = evaluateexp(stmt[1],env)
		print (exp[1],'\n')
	elif stmt[0] == "classobject":
		className = stmt[1]
		objectName = stmt[2]
		if classes.get(className) is not None:
			if objects.get(objectName) is None:
				objects[objectName] = {'className': className, 'attributes':{}}
				for attribute in classes[className]['attributes']:
					objects[objectName]['attributes'][attribute[1]] = {'type':attribute[0][1], 'value':None}
			else:
				print ("An object of the same name already exists")
				exit()
		else:
			print ("Class not declared")
			exit()
	elif stmt[0] == "ObjectAttributeAssign":
		exp = evaluateexp(stmt[3],env)
		objName = stmt[1]
		attName = stmt[2]
		if objects.get(objName) is not None:
			if objects[objName]['attributes'].get(attName) is not None:
				if objects[objName]['attributes'][attName]['type'] == exp[0]:
					objects[objName]['attributes'][attName]['value'] = exp
				else:
					print ("Types Incompatible")
					exit()
			else:
				print ("No attribute of name ",attName," exists for class ",objects[objName]['className'])
				exit()
		else:
			print ("No object declared")
			exit()


	




expression = ['int','string','double','char','bool','variable','ArrayElement','functioncall','Paranthesis','BinaryOperation','UnaryOperationL','UnaryOperationR','ObjectAttribute','objectFunctionCall']
lastEnv = 0
environments = {}
functionTrees = {} #function name -> tree associated with it. When function is called, create new env with parameters search tree, interpret
fCallCounts = {}
classes = {}
objects = {}

def main(argv):
	fileName = argv[1]
	fileObj = open(fileName,"r")
	input_string = fileObj.read()

	jslexer = lex.lex(module=parser)
	jsparser = yacc.yacc(module=grammar)
	jslexer.input(input_string)

	parse_tree = jsparser.parse(input_string,lexer=jslexer)


	for tree in parse_tree:
		interpret(tree)


if __name__ == "__main__":
	main(sys.argv)
