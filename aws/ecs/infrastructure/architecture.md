                                  Internet
                                      |
                                      ▼
                               +-------------+
                               |    ALB      |
                               | (Public SN) |
                               +-------------+
                                      |
                                      ▼
                       +-------------------------------+
                       |       ECS Fargate Tasks       |
                       |       (Private SN with NAT)   |
                       +-------------------------------+
                                      |
                                      ▼
                       +-------------------------------+
                       |     PostgreSQL Database       |
                       |        (Isolated SN)          |
                       +-------------------------------+

ALB = Application Load Balancer
SN = Subnet

Architecture Notes:
1. Public subnets (Internet-facing): Contains the Application Load Balancer
2. Private subnets (with NAT Gateway): Contains the ECS Fargate tasks that run the Express application
3. Isolated subnets (no direct internet access): Contains the RDS PostgreSQL database

Security Details:
- The database can only be accessed by the Express application (via security group rules)
- Database credentials are stored in AWS Secrets Manager and securely provided to the application
- The application has outbound internet access through the NAT Gateway
- Inbound traffic is only allowed through the Application Load Balancer
