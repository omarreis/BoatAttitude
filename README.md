# BoatAttitude
This Delphi Firemonkey 3D sample app uses phone sensors to position a sailboat. 
It is a FiremonkeySensorFusion usage sample ( see /omarreis/FiremonkeySensorFusion).
Delphi's Gyro sample app tries to do that, but does not quite get there. 

Compiled with D10.3.3 and tested on Android and iOS.

![app screenshot](BoatAttitudeShot.PNG)

## FiremonkeySensorFusion

FiremonkeySensorFusion is a cross platform (Android and iOS) 
sensor fusion ( GPS + Accelerometer + Magnetometer )
The object receives sensor readings from the phone/tablet 
sensors and outputs phone attitude  in the form
of rectangular coordinates ( aka Euler angles ).

These are also known as Yaw,Pitch,Roll or Azimuth,elevation,roll or Heading,altitude,roll.

GPS reading is used to calculate the magnetic declination,
so we can obtain the true heading from the mag heading.

In this example we use the sensor readings to orient a 3D sailboat scene. 
Also included: sea surface, cardinal points ( N,S,E and W)
Sensor readings are used to rotate a camera pointing to the boat.

## rotation quaternion
Firemonkey 3D uses TControl3D.RotationAngle vector to set object rotations. 
These are known as Euler angles. Using rotation angles in this way has a couple problems.
Setting Euler angles can lead to the so called "gymbal lock", with loss of freedom degree.
It is better to use quaternions to set object rotations.

Problem is Firemonkey TControl3D has no public method to set the quaternion directly
or manipulating the 3d matrix.  This is worked around using a helper class
(this solution was copied from https://github.com/tothpaul/Delphi/blob/master/Google%20Cardboard/4%20FullDemo/Main.pas )

     type  // helper class to help manipulating the matrix instead of changing RotationAngle
       TControl3DHelper = class helper for TControl3D
         procedure SetMatrix(const M: TMatrix3D);
       end;

     procedure TControl3DHelper.SetMatrix(const M: TMatrix3D);
     begin
       FLocalMatrix := M;
       RecalcAbsolute;
       RebuildRenderingList;
       Repaint;
     end;

This allows manipulating the object 3d rotation matrix.

Euler angle rotations are not comutative. Different order of rotations leads to different final position.  
The quaternion generated in this sample uses the plane take off sequence: first yaw while taxing,
then pitching on the runway, then rolling on the air.

    phone attitude axis ( Euler angles )
         -Y     Z       altitude X 
          |    /        heading  Y 
          |   /         roll     Z 
      /=======\
      |   | / |
      |   |/  |
      |   *---|--------- X
      |       |
      |   O   |
      \-------/

The app can set rotations using both methods (checkbox "Use quaternion")
If you use RotationAngle, you will note that if you move the phone around
and then back to original position, the boat is no longer in the
original position. Using the quaternion is better.

Check this tiktok video:

https://vm.tiktok.com/E2eQqP


