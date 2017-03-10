# SHA512Optimization
Repository for my optimization of the SHA512 algorithm on a Cyclone II DE1 FPGA board.

Development Restrictions:
8 32-bit registers for storage
Software-based hash message generation in assembly

History: The project began with a pure-software implementation and evolved into an almost pure-hardware implementation. To achieve proficiency, the system had to achieve a performance of 2,000H/s 

Intial Performance: 64H/sec
Final Performance in Directed Lab: 1786H/s

Highest score of attended class: 2,500H/s
All-time course high score: 23,000H/s

Final implementation: 43,511H/s
Final theoretical performance (without core): 70,000-80,000H/sec


Note: the contents of this repo are as they were developed and will be cleaned/organized at a later date. As the project need not be "delivered", it was simply developed on an as-is basis without regard to organizational techniques.

To future students (including URI) who might stumble upon this project: Please refer to the license before sourcing any code.
