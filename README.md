# predictingMotivation
PREDICTION OF UNDERGRADUATE STUDENTS’ COURSE MOTIVATION USING ARTIFICIAL INTELLIGENCE METHODS
For reference: 

Yavuzarslan, M. (2023). Prediction of Undergraduate Students’ Course Motivation Using Artificial Intelligence Methods. (Publication No.: 835167). PhD Dissertation, Istanbul University.

[PDF VERSION OF PhD Dissertation](https://www.researchgate.net/profile/Mithat-Yavuzarslan/publication/376456403_Yapay_Zeka_Yontemleri_ile_Universite_Ogrencilerinin_Ders_Motivasyonlarinin_Tahmin_Edilmesi_PREDICTION_OF_UNDERGRADUATE_STUDENTS'_COURSE_MOTIVATION_USING_ARTIFICIAL_INTELLIGENCE_METHODS/links/6579703a6610947889c34ad9/Yapay-Zeka-Yoentemleri-ile-Ueniversite-Oegrencilerinin-Ders-Motivasyonlarinin-Tahmin-Edilmesi-PREDICTION-OF-UNDERGRADUATE-STUDENTS-COURSE-MOTIVATION-USING-ARTIFICIAL-INTELLIGENCE-METHODS.pdf)


This repository contains the R codes I wrote for my Phd dissertation. Basically, I collected log data from undergrads using an Learning Management System (LMS).
I also used a motivational scale to obtain their level of interests towards the "Basic Computer Applications" course. The descriptions of the attributes are below:


OturumAcmaSayisi: Number of entries made by the student to the ÖYS

GecmisKonularaBakma: The number of interactions the student has with pages and content from previous weeks in the current week

Gezinme Sayısı_Toplam: Total number of pages visited by the student

GezinmeSayisi_Ortalama: Average number of pages visited in a listing, which is the total number of pages visited by the student divided by the number of logins

ToplamOturumSuresi: Total time (seconds) spent by the student in the LMS during the course period.

OrtalamaOturumSuresi: Average time spent by the student in one visit, divided by the total session duration divided by the number of logins (Seconds)

MateryalIndirme: Number of times a student has downloaded shared files related to a topic or assignment

OdevDenemeSayisi: The number of interactions the student has with the pages and content related to the assignment

OdeveHarcananZaman: Time spent by the student on the assignment page (seconds)

SayfadaKalma_Ortalama: Average time a student spends on a page divided by total session time by total browsing (Seconds)

SınavOdakliCalisma: Time spent by the student in LMS during exam weeks (seconds)

VideoIzlemeSuresi: Time (seconds) student spends on pages with video content

YuklenenOdev: Number of homework uploaded by the student to the system

SınavNotu: Student's end-of-term grade point average

Devamsizlik: Hours of absence during the semester

Faculty: Faculty to which the student is enrolled

Class: Information on which grade the student is in

Gender: Student's gender

OgretimTuru: The way the course is taught in the relevant semester (distance or hybrid)

________________________________________
Motivasyon (Target Attribute): The total score of the items answered by the student on the EES


In buildAndTest_N207.R, after some preprocessing steps, kNN, Naive Bayes, Support Vector Machines, CART, C5.0, Random Forest and Logistic Regression algorithms were used to build the models.
Building process were repeated for each data set generated with Oversampling, SMOTE and MWMOTE techniques. Testing analysis were conducted with holdout method (70%-30%).

In buildAndTest_N207_kFold.R, k-fold cross validation (with k value 5 and 10) were applied on the main dataset with 207 samples to test kNN, Naive Bayes, Support Vector Machines, CART, C5.0, Random Forest and Logistic Regression models.


In validation_N200.R, a new seperate dataset containing 200 samples were used for validating the models.

In validation_N200_kFold.R, k-fold cross validation (with k value 5 and 10) were applied on the new dataset with 200 samples to test kNN, Naive Bayes, Support Vector Machines, CART, C5.0, Random Forest and Logistic Regression models.

English Abstract of the dissertation:

"In online learning environments, determining students' interest in the course is more difficult compared to face-to-face learning environments. On the other hand, Learning Management Systems (LMS) create and store record data called logs of students' interactions with the environment and materials offered online. It is possible to reveal the motivation levels of students towards the course and the variables that affect their motivation levels by processing this data with artificial intelligence methods. This study aims to predict the motivational levels of university students towards a course using LMS log data. The research was conducted with 407 students registered for the Basic Computer Applications course opened in the Fall 2020-2021, Spring 2020-2021, and Fall 2021-2022 semesters at a university. Log records stored in the Moodle-based LMS server where students were registered for the course during the 14 weeks covering the course period were obtained from the students. Additionally, the Course Interest Scale (CIS), based on the ARCS motivation theory developed by John M. Keller, was adapted to Turkish and applied within the scope of the study to determine the students' motivation levels towards the course. 
In the study, two datasets were used considering students' LMS usage intensity and the branches they are registered for. Modeling and testing were carried out with the first data set, and the second data set was used as a validation set for the models developed with the first data set. The motivation target was obtained by reducing the total scores obtained from CIS to binary and triple classes. Artificial intelligence models used in the study were trained with k-Nearest Neighbor, Naive Bayes, Support Vector Machines, C5.0, CART, Random Forest, and Logistic Regression algorithms. Random subsampling, k-fold cross-validation, and hold-out methods were used for performance evaluation, and oversampling methods were applied to the dataset in the analyses carried out with hold-out. Accuracy and ROC values were specifically used as performance metrics.
As a result of the research, it was seen that it is possible to predict students' motivation levels towards the course using the data obtained from the LMS log files. In this direction, while around 70% overall classification success was observed with random subsampling and k-fold cross-validation in the first and second stage findings, the performances of the models tested with hold-out using the oversampled datasets were measured close to 80%. On the other hand, the highest performances among the models tested, especially in the first stage, were obtained with decision tree algorithms (C5.0, CART, and Random Forest). To determine the features that affect the motivation target property, the variable importance rankings of the algorithms with high performance were examined, and it was observed that features expressing duration such as "TotalSessionTime," "ExamFocusedStudy," and "AverageTimeOnPage" from the features created with LMS log files were effective on students' motivation towards the course. The results of the research were discussed considering the findings in the relevant literature and suggestions for similar studies to be carried out in the future were presented."

Reference:
Yavuzarslan, M. (2023). Prediction of Undergraduate Students’ Course Motivation Using Artificial Intelligence Methods. (Publication No.: 10540483). PhD Dissertation, Istanbul University.



