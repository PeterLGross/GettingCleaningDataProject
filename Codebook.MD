Project CodeBook
================

# Original Table
## ("mean std table" from script)

Column numbers denoted in [] brackets.

[1] Subject_ID - Number identifying the test subject to which the record applies.

[2] Activity_Name - Name of physical activity being recorded. Can take the following values: LAYING,
SITTING, STANDING, WALKING, WALKING DOWNSTAIRS, or WALKING UPSTAIRS

[3] tBodyAcc.mean_X - Mean of time domain signal representing acceleration of the subject's body
on the x-axis.

[4] tBodyAcc.mean_Y - Mean of time domain signal representing acceleration of the subject's body
on the y-axis.

[5] tBodyAcc.mean_Z - Mean of time domain signal representing acceleration of the subject's body
on the z-axis.

[6] tBodyAcc.std_X - Standard deviation of time domain signal representing acceleration of the
subject's body on the x-axis.

[7] tBodyAcc.std_Y - Standard deviation of time domain signal representing acceleration of the
subject's body on the y-axis.

[8] tBodyAcc.std_Z - Standard deviation of time domain signal representing acceleration of the
subject's body on the z-axis.

[9] tGravityAcc.mean_X - Mean of time domain signal representing acceleration due to gravity
on the x-axis.

[10] tGravityAcc.mean_Y - Mean of time domain signal representing acceleration due to gravity
on the y-axis.

[11] tGravityAcc.mean_Z - Mean of time domain signal representing acceleration due to gravity
on the z-axis.

[12] tGravityAcc.std_X - Standard deviation of time domain signal representing acceleration
due to gravity on the x-axis.

[13] tGravityAcc.std_Y - Standard deviation of time domain signal representing acceleration
due to gravity on the y-axis.

[14] tGravityAcc.std_Z - Standard deviation of time domain signal representing acceleration
due to gravity on the z-axis.

[15] tBodyAccJerk.mean_X - Mean of time domain signal representing jerk on subject's body
on the x-axis. Jerk calculated using derivative of the body's linear acceleration.

[16] tBodyAccJerk.mean_Y - Mean of time domain signal representing jerk on subject's body
on the y-axis. Jerk calculated using derivative of the body's linear acceleration.

[17] tBodyAccJerk.mean_Z - Mean of time domain signal representing jerk on subject's body
on the z-axis. Jerk calculated using derivative of the body's linear acceleration.

[18] tBodyAccJerk.std_X - Standard deviation of time domain signal representing jerk on
subject's body on the x-axis. Jerk calculated using derivative of the body's linear acceleration.

[19] tBodyAccJerk.std_Y - Standard deviation of time domain signal representing jerk on
subject's body on the y-axis. Jerk calculated using derivative of the body's linear acceleration.

[20] tBodyAccJerk.std_Z - Standard deviation of time domain signal representing jerk on
subject's body on the z-axis. Jerk calculated using derivative of the body's linear acceleration.

[21] tBodyGyro.mean_X - Mean of time domain signal from gyroscope representing the angular
velocity of the subject's body on the x-axis.

[22] tBodyGyro.mean_Y - Mean of time domain signal from gyroscope representing the angular
velocity of the subject's body on the y-axis.

[23] tBodyGyro.mean_Z - Mean of time domain signal from gyroscope representing the angular
velocity of the subject's body on the z-axis.

[24] tBodyGyro.std_X - Standard deviation of time domain signal from gyroscope representing
the angular velocity of the subject's body on the x-axis.

[25] tBodyGyro.std_Y - Standard deviation of time domain signal from gyroscope representing
the angular velocity of the subject's body on the y-axis.

[26] tBodyGyro.std_Z - Standard deviation of time domain signal from gyroscope representing
the angular velocity of the subject's body on the z-axis.

[27] tBodyGyroJerk.mean_X - Mean of time domain signal representing angular jerk on subject's
body on the x-axis. Angular jerk calculated using the body's angular velocity.

[28] tBodyGyroJerk.mean_Y - Mean of time domain signal representing angular jerk on subject's
body on the y-axis. Angular jerk calculated using the body's angular velocity.

[29] tBodyGyroJerk.mean_Z - Mean of time domain signal representing angular jerk on subject's
body on the z-axis. Angular jerk calculated using the body's angular velocity.

[30] tBodyGyroJerk.std_X - Standard deviation of time domain signal representing angular jerk
on subject's body on the x-axis. Angular jerk calculated using the body's angular velocity.

[31] tBodyGyroJerk.std_Y - Standard deviation of time domain signal representing angular jerk
on subject's body on the y-axis. Angular jerk calculated using the body's angular velocity.

[32] tBodyGyroJerk.std_Z - Standard deviation of time domain signal representing angular jerk
on subject's body on the z-axis. Angular jerk calculated using the body's angular velocity.

[33] tBodyAccMag.mean - Mean of the time domain signal representing the magnitude of the
acceleration of the subject's body.

[34] tBodyAccMag.std - Standard deviation of the time domain signal representing the
magnitude of the acceleration of the subject's body.

[35] tGravityAccMag.mean - Mean of the time domain signal representing the magnitude of
the acceleration due to gravity.

[36] tGravityAccMag.std - Standard deviation of the time domain signal representing the
magnitude of the acceleration due to gravity.

[37] tBodyAccJerkMag.mean - Mean of the time domain signal representing the magnitude of
the jerk on the subject's body.

[38] tBodyAccJerkMag.std - Standard deviation of the time domain signal representing the
magnitude of the jerk on the subject's body.

[39] tBodyGyroMag.mean - Mean of the time domain signal of the magnitude of the angular
velocity of the subject's body measured by the gyroscope.

[40] tBodyGyroMag.std - Standard deviation of the time domain signal of the magnitude
of the angular velocity of the subject's body measured by the gyroscope.

[41] tBodyGyroJerkMag.mean - Mean of time domain signal of the magnitude of the angular
jerk on the subject's body calculated using the angular velocity.

[42] tBodyGyroJerkMag.std - Standard deviation of time domain signal of the magnitude
of the angular jerk on the subject's body calculated using the angular velocity.

[43] fBodyAcc.mean_X - Mean of the frequency domain signal of the acceleration of the
subject's body on the x-axis. Obtained using a Fast Fourier Transform (FFT).

[44] fBodyAcc.mean_Y - Mean of the frequency domain signal of the acceleration of the
subject's body on the y-axis. Obtained using a Fast Fourier Transform (FFT).

[45] fBodyAcc.mean_Z - Mean of the frequency domain signal of the acceleration of the
subject's body on the z-axis. Obtained using a Fast Fourier Transform (FFT).

[46] fBodyAcc.std_X - Standard deviation of the frequency domain signal of the
acceleration of the subject's body on the x-axis. Obtained using a Fast Fourier
Transform (FFT).

[47] fBodyAcc.std_Y - Standard deviation of the frequency domain signal of the
acceleration of the subject's body on the y-axis. Obtained using a Fast Fourier
Transform (FFT).

[48] fBodyAcc.std_Z - Standard deviation of the frequency domain signal of the
acceleration of the subject's body on the z-axis. Obtained using a Fast Fourier
Transform (FFT).

[49] fBodyAccJerk.mean_X - Mean of the frequency domain signal representing the
jerk on the subject's body on the x-axis. Obtained using a Fast Fourier Transform (FFT).

[50] fBodyAccJerk.mean_Y - Mean of the frequency domain signal representing the
jerk on the subject's body on the y-axis. Obtained using a Fast Fourier Transform (FFT).

[51] fBodyAccJerk.mean_Z - Mean of the frequency domain signal representing the
jerk on the subject's body on the z-axis. Obtained using a Fast Fourier Transform (FFT).

[52] fBodyAccJerk.std_X - Standard deviation of the frequency domain signal representing the
jerk on the subject's body on the x-axis. Obtained using a Fast Fourier Transform (FFT).

[53] fBodyAccJerk.std_Y - Standard deviation of the frequency domain signal representing the
jerk on the subject's body on the y-axis. Obtained using a Fast Fourier Transform (FFT).

[54] fBodyAccJerk.std_Z - Standard deviation of the frequency domain signal representing the
jerk on the subject's body on the z-axis. Obtained using a Fast Fourier Transform (FFT).

[55] fBodyGyro.mean_X - Mean of the frequency domain signal representing the angular velocity
of the subject's body on the x-axis. Obtained using a Fast Fourier Transform (FFT).

[56] fBodyGyro.mean_Y - Mean of the frequency domain signal representing the angular velocity
of the subject's body on the y-axis. Obtained using a Fast Fourier Transform (FFT).

[57] fBodyGyro.mean_Z - Mean of the frequency domain signal representing the angular velocity
of the subject's body on the z-axis. Obtained using a Fast Fourier Transform (FFT).

[58] fBodyGyro.std_X - Standard deviation of the frequency domain signal representing
the angular velocity of the subject's body on the x-axis. Obtained using a Fast Fourier
Transform (FFT).

[59] fBodyGyro.std_Y - Standard deviation of the frequency domain signal representing
the angular velocity of the subject's body on the y-axis. Obtained using a Fast Fourier
Transform (FFT).

[60] fBodyGyro.std_Z - Standard deviation of the frequency domain signal representing
the angular velocity of the subject's body on the z-axis. Obtained using a Fast Fourier
Transform (FFT).

[61] fBodyAccMag.mean - Mean of the frequency domain signal representing the magnitude
of the acceleration of the subject's body. Obtained using a Fast Fourier Transform (FFT).

[62] fBodyAccMag.mean - Standard deviation of the frequency domain signal representing
the magnitude of the acceleration of the subject's body. Obtained using a Fast Fourier
Transform (FFT).

[63] fBodyBodyAccJerkMag.mean - Mean of the frequency domain signal representing the
magnitude of the jerk on the subject's body. Obtained using a Fast Fourier Transform (FFT).

[64] fBodyBodyAccJerkMag.std - Standard deviation of the frequency domain signal
representing the magnitude of the jerk on the subject's body. Obtained using a Fast
Fourier Transform (FFT).

[65] fBodyBodyGyroMag.mean - Mean of the frequency domain signal of the magnitude of the
angular velocity of the subject's body measured by the gyroscope. Obtained using a Fast
Fourier Transform (FFT).

[66] fBodyBodyGyroMag.std - Standard deviation of the frequency domain signal of the
magnitude of the angular velocity of the subject's body measured by the gyroscope.
Obtained using a Fast Fourier Transform (FFT).

[67] fBodyBodyGyroJerkMag.mean - Mean of the frequency domain signal of the magnitude
of the angular jerk on the subject's body calculated using the angular velocity. Obtained
using a Fast Fourier Transform (FFT).

[68] FBodyBodyGyroJerkMag.std - Standard deviation of the frequency domain signal of the
magnitude of the angular jerk on the subject's body calculated using the angular velocity.
Obtained using a Fast Fourier Transform (FFT).

# Tidy Data Table
## (Output from script)

Column names are identical in this table to the original table except that the measurement
variable columns (columns 3 - 68) contain the _MEAN suffix indicating that the numeric
amount for a record in that column contains the mean for that particular measurement across
the Subject ID and Activity Name combination.