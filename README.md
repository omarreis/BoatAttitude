# BoatAttitude
This Delphi Firemonkey 3D sample app uses phone sensors to position a sailboat. 
It is a FiremonkeySensorFusion usage sample ( see /omarreis/FiremonkeySensorFusion).
Delphi's *Gyroscope* sample app tries to do that, but does not quite gets there. 

("BoatAttitude" is a bad name since what changes is the camera orientation).

Compiled with D10.3.3 and tested on Android and iOS.

Oct/20: fixed issues with sensors with  D10.4.1  ( iOS and Android )

![app screenshot](BoatAttitudeShot.PNG)

Tags: #Delphi #Firemonkey #Sensors #iOS #Android

## FiremonkeySensorFusion

FiremonkeySensorFusion is a cross platform (Android and iOS) sensor fusion ( GPS + Accelerometer + Magnetometer )
The object receives sensor readings from the phone/tablet sensors and outputs phone attitude  in the form
of rectangular coordinates (aka Euler angles ).

These are also known as Yaw,Pitch,Roll or Azimuth,elevation,roll or Heading,altitude,roll.
This profusion of names causes some confusion.

GPS reading is used to calculate the magnetic declination (using WMM model), 
so we can obtain the true heading from the magnetic heading.

In this example we use the sensor readings to orient a 3D sailboat scene. 
Also included in the scene: sea surface and cardinal points (N,S,E and W)
Sensor readings are used to rotate a camera pointing to the boat.

## Using rotation quaternion
Firemonkey 3D uses TControl3D.RotationAngle vector to change object rotation. 
These are known as Euler angles. Using rotation angles in this way has a couple problems.
Setting Euler angles can lead to the so called "gymbal lock", with loss of degree of freedom.
It is better to use quaternions to set object rotations.

Problem is Firemonkey FMX TControl3D has no public method to set the quaternion directly
or manipulating the 3d matrix.  This is worked around using a helper class
(the solution was copied from https://github.com/tothpaul/Delphi/blob/master/Google%20Cardboard/4%20FullDemo/Main.pas )

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

This allows manipulating the object 3d rotation matrix directly.

Euler angle rotations are not commutative. Different order of rotations leads to different final position.  
The quaternion generated in this sample uses the airplane take off sequence: first *yaw* while taxing,
then *pitch* on the runway, then *roll* on the air.

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

This app can set rotations using both methods (checkbox "Use quaternion")
If you use RotationAngle, you will note that if you move the phone around
and then return to original position, the boat is no longer with the same attitude. 
Bottom line: use quaternion for setting object rotations.

## dependencies

* DelphiWorlds Kastri-free - https://github.com/DelphiWorlds/KastriFree
* FiremonkeySensorFusion - https://github.com/omarreis/FiremonkeySensorFusion 

## tiktok videos:
* BoatAttitude: https://vm.tiktok.com/E2eQqP
* AirlinerAttitude w/ FiremonkeySensorFusion and TFlapLabel: https://www.tiktok.com/@omar_reis/video/6846360497550380294

## AirlinerAttitude
AirlinerAttitude app (also in  this repository) is a similar sample app, this time featuring a airliner plane model.
Phone attitude sensors control:
* phone true heading rotates the compass disk.  
* airplane yaw is set to roll/2, so it points generally away from the phone, but we can see the plane side as it rolls (like a real plane would do, I suppose).
* otherwise, airplane pitch and roll same as phone's.

![app screenshot](AirlinerAttitudeShot.PNG)

...

