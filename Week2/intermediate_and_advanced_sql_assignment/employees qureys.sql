#If the average salary for males and females are not equal, then offer a higher bonus to
#compensate for this circumstance.
#If the average salary for males and females are not equal, then you can reduce the salaries
#of the gender paid higher and state how much funds were saved with this plan/practice.

SELECT 
    e.gender, s.salary
FROM
    employees e
        JOIN
    salaries s ON e.emp_no = s.emp_no
GROUP BY gender;

# we can see from the result, we found that the average salaries of men are $ 14 less than the average wages of women

# Number of male and female employees in the company.
SELECT gender, COUNT(gender)
FROM employees
GROUP BY gender;


#The ratio of males to females hired in the last 5 years.
SELECT YEAR(hire_date) year, gender, COUNT(gender)
FROM employees
GROUP BY year, gender
ORDER BY year desc;

#the number hireing male and female in each departments in last five years
SELECT YEAR(e.hire_date) year, d.dept_name, e.gender, count(e.gender)
FROM
	salaries s
		JOIN
	employees e ON s.emp_no = e.emp_no
		JOIN
    dept_emp de ON e.emp_no = de.emp_no
		JOIN
    departments d ON de.dept_no = d.dept_no
WHERE e.hire_date BETWEEN '1996-12-31' AND '2000-12-31'
GROUP BY year, d.dept_name, e.gender
ORDER BY year desc;


#the departments that have the highest gaps in descending order.
SELECT d.dept_name, e.gender, COUNT(e.gender)
FROM
	employees e 
        JOIN
    dept_emp de ON e.emp_no = de.emp_no
        JOIN
    departments d ON de.dept_no = d.dept_no
GROUP BY d.dept_name, e.gender;


#the average salary for male and female in each departments
SELECT d.dept_name,e.gender,
    AVG(s.salary) AS average_salary
FROM
	salaries s
		JOIN
	employees e ON s.emp_no = e.emp_no
		JOIN
    dept_emp de ON e.emp_no = de.emp_no
		JOIN
    departments d ON de.dept_no = d.dept_no
GROUP BY d.dept_name, e.gender;

select dept_name, count(dept_no)
from departments
group by dept_name;



    














