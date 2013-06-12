twizzer
=======

This package is developed as a final project of CS 583 Functional Language, Oregon State University.

Program name: Twizzer

Date: 6/10/2013

Authors: Shan Xue, Lu Qin and Qi Lou.

Features
-------------------------------------------------------------------------------------------
* Implemented using functional language-Haskell.
* No need to install or setup environment. Just click on the excutable file!
* No maintainance is needed on the server side. Just keep it running!
* Complete user control
   * Permission control is implemented.
     Any user with permission 1 is an administrator. Any user with permission 0 is student/common user.
   * The administrator can manage users by adding accounts. 
   * Any user can freely update their password.
* An easy-use platform for the administrator to create twizzes, collect solutions, collect reviews and
  tabulate the final score (taking both solutions and reviews into account). 
* Provides different functions for students/common users at different phases.

Specification
-------------------------------------------------------------------------------------------
* Server side: 
  * Keep the server program running.
  * Accounts.txt saves all the username, password and permission, which must be seperated by "\t". 
    1 indicates ADMIN and 0 represents STUDENT.
  * TwizLog.txt saves the index and deadlines of current active twiz. We assume that a twiz is active since 
  it is created until the next twiz is created. 

* Client side: 
    Initially, the user is asked for username and password to log in. Then according to current phase,
    the user selects a task from the printed out menu by entering the corresponding number. This
    program accomplishes the tasks based on files. In particular, the twiz/solution/review/score file that the user
    would like to get is always automatically saved to a file in current working directory; the solution/
    review the user would like to submit is supposed to be a file, the name of which Twizzer would
    need during the submission. 
    * Functions for the administrator
        * -0- ChangePassword
        * -1- Add an account
        * -2- CreateTwiz
        * -3- GetTwiz
        * -4- ReadAllSolution
        * -5- ReadAllReview
        * -6- CalculateScore
        * -7- Exit
    * Functions for common user/student
      * when phase is Task
          * -0- ChangePassword
          * -1- GetTwiz
          * -2- SubmitSolution
          * -3- Exit
      * when phase is Review
    	    * -0- ChangePassword
          * -1- GetTwiz
          * -2- ReadSolution
          * -3- SubmitReview
          * -4- Exit
      * when phase is Idle
    	   * -0- ChangePassword
         * -1- GetTwiz
         * -2- ReadReview
         * -3- Exit	

How to run
-------------------------------------------------------------------------------------------
On the server side, download folder .bin/server-side, open Server.exe and keep running.

On the clienct side, download folder .bin/client-side, open Client.exe, type server IP address, do whatever operations.

Their also exist some test files for test the software if needed.



