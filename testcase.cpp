
int add(int a, int b)
{ 
	int result=a+b;
	return result; 
}

int pow(int num, int power)
{
	if(power==0)
	{
		return 1;
	}
	else
	{
		return num*pow(num, power-1);
	}
}

class Shape
{
	public:
	string name;
	int corners;


};


int main()
{

cout << "Case 1: Data Types" <<endl;

int a=12; char first='a'; char second='b';
double b; double c = 9.75; b=4.5;
bool d=true;
char test; test='x';
string test_string="CPython!!";

cout << test_string << endl;

test_string="This is a Python-based C++ interpreter";

cout << a << endl;
cout << b << endl;
cout << c << endl;
cout << d << endl;
cout << first << endl;
cout << second << endl;
cout << test << endl;
cout << test_string << endl;

cout << "--------------------------------------------------------------------" << endl;
cout << "Case 2: Class Definitions" << endl;

Shape Circle;
Circle.name="Circle";
Circle.corners=0;
cout << "--------------------------------------------------------------------" << endl;
cout << "Case 3: IF Conditions" << endl;

if(a<5)
{  
	if(a>1)
	{
		cout << "a>1" << endl;
	}
	else if(a==1)
	{
		cout << "a=1" << endl;
		a++;
	}
	else
	{
		cout << "a<1" << endl;
	}
}
else
{ 
	a++; 
}

cout << a << endl;
cout << "--------------------------------------------------------------------" << endl;
cout << "Case 4: Arithmetic Operations" << endl;

a=2*((3+5-4)/2);


int sum=0;
for(int i=2; i < 10; i++)
{
	sum=i+a;
}

cout << sum << endl;
cout << "--------------------------------------------------------------------" << endl;
cout << "Case 5: Loops" << endl;

for(int i=0;i<3;i++)
{
	for(int j=0;j<2;j++)
	{
		for(int k=0;k<2;k++)
		{
			cout << "Testing For Loop" << endl;
		}
	}
}

cout << "--------------------------------------------------------------------" << endl;
cout << "Case 6: Function Calls" <<endl;

c=3.5;
if(c==2.5)
{ 
	test='o';
}
else if(c>3)
{ 
	a=pow(2,3);
}
else
{ 
	a=5;
}

cout << "--------------------------------------------------------------------" << endl;
cout << "Case 7: Arrays" << endl;

int array[9]={1,2,3,4,5,6,7,8,9}; 
char secondArray[]={'a','b','c'}; 
a=9;
int p,q;
for(int l=0;l<a;l++)
{
	p=(array[l])%10;
	array[l]=p;
	cout << array[l] << endl;
}

cout << "--------------------------------------------------------------------" << endl;
}