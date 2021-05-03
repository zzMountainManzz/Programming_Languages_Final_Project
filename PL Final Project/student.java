class Student
{
    private String name;
    private int age;

    public Student (String n, int a)
    {
        name = n;
        age = a;
    }

    public String getName ()
    {
        return name;
    }

    public void setName (String n)
    {
        this.name = n;
    }

    public int getAge ()
    {
        return age;
    }

    public void setAge (int a)
    {
        this.age = a;
    }
}

public class main 
{
        public static void main(String[] args) 
    {
        Student Jacob = new Student("Jacob",100);
        Student Wolfe = new Student("Wolfe",999);
        Student Luke = new Student("Luke",2);

        Jacob.setAge(Wolfe.getAge() - Luke.getAge());

        System.out.print(Jacob.getAge()); 

    }
}

    © 2021 GitHub, Inc.
    Terms
    Privacy
