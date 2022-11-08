USE wesellstuffltd;

-- Simulated Organization:
	-- National wholesalers. We have both corporate and sales employees and are trying to maximize distribution among 12 different states.


-- Questions:
	-- What are the top three states we sell to by volume vs. by transaction amount? 
			SELECT StateID
			
            FROM customers;
-- Select Statements:
				-- Select Statement 1: What city houses the majority of our customers? Dallas
					/* Understanding the demographics of our customer base is important. We can focus research on this group to investigate consumer behavior. */
						SELECT city,COUNT( DISTINCT customer_ID) AS 'Customer Count'
                        FROM customers
                        GROUP BY city
                        ORDER BY COUNT(DISTINCT customer_ID) DESC;
                        
				-- Select Statement 2: Which products have fewer than 50 left in stock?
					/* By understanding which products are consistently running low in stock, we can better understand consumer needs and product popularity. This will allow us to maximize promotional activities by spotlighting less popular products to stimulate growth. */
						SELECT id, quantityInStock
                        FROM products
                        WHERE quantityInStock < 50
                        ORDER BY quantityInStock DESC;
						
				-- Join statement 1: What city/state buys the greatest volume of goods from us? Grand Junction
					/* This query is important for our business to understand which cities are consuming the highest volume of our goods. By udnerstanding this metric, we will be better able to allocate promotional activities and optimize spending. */
						SELECT c.city , s.Quant_sold AS 'Total Volume Sold'
						FROM customers as c
						JOIN salesdata as s 
						ON c.customer_ID = s.custID
						GROUP BY c.city
						ORDER BY s.quant_sold DESC;

				-- Join statement 2: What state consumes the most by volume? Colorado
					/* Similar to knowing which city consumes the greatest volume of goods, the state allows us to see how buying trends are distinct across the nation. */
						SELECT st.FullName , s.Quant_sold AS 'Total Volume Sold'
						FROM customers as c
						JOIN salesdata as s 
						ON c.customer_ID = s.custID
						JOIN stateinfo as st
						ON c.stateID = st.StateID
						GROUP BY c.stateID
						ORDER BY s.Quant_sold DESC;
                        
				-- Subquery Statement: How many people are earning under the average salary at this company? OR what percent of people?
					/* Investigating how well our employees are paid is an essential business practice. We want our corporate employees to be happy and productive. */
								SELECT COUNT(employeeID)
                                FROM corp_employees
                                WHERE salary < 
													(SELECT AVG(salary)
															FROM corp_employees)
								;
                                
				-- Subquery Statement 2: What percent of our corproate emloyees are earning under the average salary?
					/* After investigating this, we discover that 54% of all corporate employees are earning less than the average salary. We may need to reinvestigate our benefits. */
                                	SELECT ((SELECT COUNT(employeeID)
                                FROM corp_employees
                                WHERE salary < 
													(SELECT AVG(salary)
															FROM corp_employees))/ COUNT(employeeID)*100) AS "Percent of Corporate Employees Earning Less than Company Average Salary"
								FROM corp_employees;
                                
                             
                                
				-- Subquery Statement 3:  What is the highest group earning less than the average salary by gender?
					/* Here, we are trying to investigate whether there is any pay discrimination based on gender. Based on our findings, there is somewhat of a discrepancy between gender groups based on pay. */
						SELECT g.genderdesc, COUNT(c.employeeID) as 'Number of Identity Earning under the Average Salary'
                                FROM corp_employees as c
							JOIN genderids AS g
                        ON c.genderid = g.genderid
                                WHERE salary < 
													(SELECT AVG(salary)
															FROM corp_employees)
					
						GROUP BY genderdesc
                        ORDER BY  COUNT(c.employeeID) DESC;
                                
							
							
                        
				-- Join Statement (in progress): Which sales rep has sold the most revenue( SUM(products.priceEach * salesdata.Quant_Sold) to date?
						SELECT CONCAT(sa.first_name, ' ', sa.last_name),SUM(p.priceEach * s.Quant_Sold) AS 'Yearly Revenue', s.Sales_Rep_ID
						FROM salesdata as s
						JOIN products as p
						ON s.product_id = p.id
						JOIN sales_employees as sa
						ON s.sales_rep_id = sa.salesperson_id_num
						GROUP BY sales_rep_id
						ORDER BY 'Yearly Revenue' ASC;
   
   
-- Stored Functions: 
	
    -- Stored Function 1: Convert all names in the customers category into first and last names to print out customer addresses for PR purposes:
		USE wesellstuffltd;
                            DELIMITER $$
										CREATE FUNCTION Customer_Full_Name
											(firstname VARCHAR(50),
											lastname VARCHAR(50))
										RETURNS VARCHAR(100)
										DETERMINISTIC
										BEGIN 
											DECLARE Customer_Full_Name VARCHAR(100);
											SET Customer_Full_Name = CONCAT(firstname, ' ' ,lastname);
											RETURN (Customer_Full_Name);
							END $$
                            
							SELECT Customer_Full_Name(first_name, last_name)
								AS Customer_Full_Name, address1, address2, address3, city, s.fullname as 'State'
						
								FROM customers AS c
							JOIN stateinfo as s
                            on c.stateID = s.stateid
                            ORDER BY customer_full_name ASC;
                            
-- Stored Procedure: This function will select only the customers who reside in a specific city. We can use this fpr marketing purposes.
		CREATE PROCEDURE select_customers_who @City nvarchar(45)
        AS SELECT * FROM customers where city = @city
        G0;
        
USE wesellstuffltd;

        


    