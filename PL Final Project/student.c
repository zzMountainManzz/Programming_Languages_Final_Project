#include <stdio.h>

struct student {
    char* name;
    int age; 
};
 
struct student student_student(char* n, int a) 
{
  
  struct student newStudent;
  newStudent.name = n; // probably want to copy all of the string
  newStudent.age = a;
  return newStudent; 

}

char* student_getName(struct student s) {
    return s.name; 
}

void student_setName(struct student *s, char* n) {
    //this.name = n;
    s->name = n;
}

int student_getAge(struct student s) {
    return s.age;
}

void student_SetAge(struct student *s, int a) {
    s->age = a; 
}


int main()
{
    struct student Jacob = student_student("jacob",100);
    struct student Wolfe = student_student("wolfe",999);
    struct student Luke = student_student("Luke",2);
    student_setAge (&Jacob,student_getAge(Wolfe) - student_getAge(Luke));
    
    printf("%d\n",student_getAge(Jacob)); 
    
    return 0;  

}