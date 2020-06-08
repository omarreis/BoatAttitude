unit fAirlinerAttitude;    // 3d plane scene w/ airliner plane attitude controlled by phone sensors
// --- by oMAR jun20 --//
// https://github.com/omarreis/AirlinerAttitude
//
// also see https://github.com/omarreis/BoatAttitude
//-----------------------------------------------------------------------------

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms3D, FMX.Types3D, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  System.Math.Vectors,
  FMX.Objects3D, FMX.MaterialSources, FMX.Controls3D, FMX.Controls.Presentation, FMX.StdCtrls,
  FMX.Layers3D,
  FMX.Platform,

  {$IFDEF ANDROID}
  FMX.Platform.Android,
  DW.PermissionsRequester,   // Must have /D|elphiWorlds/KastriFree for Android permissions
  DW.PermissionsTypes,       // Android API Level 26+ permissions handling
  {$ENDIF ANDROID}
  MagnetometerAccelerometerFusion,  // TMagnetoAccelerometerFusion
  FMX.Media;

type
  TFormAirlinerAttitude = class(TForm3D)
    Light1: TLight;
    LightMaterialSource1: TLightMaterialSource;
    layerDisplay: TLayer3D;
    Label1: TLabel;
    tbX: TTrackBar;
    tbY: TTrackBar;
    tbZ: TTrackBar;
    labX: TLabel;
    labY: TLabel;
    labZ: TLabel;
    Camera1: TCamera;
    dummyCameraGroup: TDummy;
    labAttitude: TLabel;
    BoatMat01: TLightMaterialSource;
    Mesh1: TMesh;
    cbInvert: TSwitch;
    Label2: TLabel;
    textNorth: TText3D;
    diskSeaHorizon: TDisk;
    colorMaterialSourceSeaColor: TColorMaterialSource;
    textEast: TText3D;
    textWest: TText3D;
    textSouth: TText3D;
    modelAirliner: TModel3D;
    dummyAirliner: TDummy;
    dummyDummy: TDummy;
    dummySeaDisk: TDummy;
    colorMaterialSourcePlane: TColorMaterialSource;
    modelAirlinerMat01: TLightMaterialSource;
    procedure Form3DCreate(Sender: TObject);
    procedure Form3DActivate(Sender: TObject);
  private
    fDefaultRotation:TVector3D;
    fMagAccelFusion:TMagnetoAccelerometerFusion;
    {$IFDEF Android}
    FRequester: TPermissionsRequester;
    {$ENDIF Android}
    procedure FusionSensorHeadingAltitudeChanged(Sender:TObject);
    function  AppEventHandler(AAppEvent: TApplicationEvent;  AContext: TObject): Boolean;
    {$IFDEF Android}
    procedure PermissionsResultHandler(Sender: TObject; const ARequestCode: Integer; const AResults: TPermissionResults);
    {$ENDIF Android}
  public
  end;

var
  FormAirlinerAttitude: TFormAirlinerAttitude;

implementation

{$R *.fmx}

{$IFDEF Android}  // Android permissions
const
  cPermissionsBoatAttitude=3;
  cPermissionAccessCoarseLocation = 'android.permission.ACCESS_COARSE_LOCATION';
  cPermissionAccessFineLocation   = 'android.permission.ACCESS_FINE_LOCATION';
{$ENDIF Android}  // request permissions to work

function deg2rad(const d:Single):Single;
begin
  Result := d/180*3.141592;
end;

// from wikipedia https://en.wikipedia.org/wiki/Conversion_between_quaternions_and_Euler_angles
// by experience I found that this is more like ToQuaternion(roll,pitch,yaw)   or (z,x,y)
// confusing ..
procedure  ToQuaternion(const yaw,pitch,roll:Single; var q:TQuaternion3D);   // yaw (Z), pitch (Y), roll (X)
var cy,sy,cp,sp,cr,sr,y,p,r:Single;
begin
  // Abbreviations for the various angular functions
  y := deg2rad(yaw);   // deg to rad
  p := deg2rad(pitch);
  r := deg2rad(roll);

  cy := cos(y * 0.5);   sy := sin(y * 0.5);
  cp := cos(p * 0.5);   sp := sin(p * 0.5);
  cr := cos(r * 0.5);   sr := sin(r * 0.5);

  q.RealPart   := cr * cp * cy + sr * sp * sy;
  q.ImagPart.x := sr * cp * cy - cr * sp * sy;
  q.ImagPart.y := cr * sp * cy + sr * cp * sy;
  q.ImagPart.z := cr * cp * sy - sr * sp * cy;
end;

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

procedure TFormAirlinerAttitude.Form3DCreate(Sender: TObject);
var AppEventSvc: IFMXApplicationEventService;
   {$IFDEF IOS} AEService: IFMXApplicationEventService;  {$ENDIF IOS}
begin
  //create sensors
  fMagAccelFusion := TMagnetoAccelerometerFusion.Create(Self);           //use sensor fusion
  //fMagAccelFusion.OnAccelerometerChange  := FusionSensorAccelChanged;  //not using those
  //fMagAccelFusion.OnMagnetometerChange   := FusionSensorMagChanged;
  fMagAccelFusion.OnHeadingAltitudeChange:= FusionSensorHeadingAltitudeChanged; // attitude change handler

  fDefaultRotation := TVector3D.Create(0,0,0);

  {$IFDEF ANDROID}
  if TPlatformServices.Current.SupportsPlatformService(IFMXApplicationEventService, IInterface(AppEventSvc)) then
    AppEventSvc.SetApplicationEventHandler(AppEventHandler);

  //permission requester for Android API 26+ permissions
  FRequester := TPermissionsRequester.Create;
  FRequester.OnPermissionsResult := PermissionsResultHandler;
  {$ENDIF ANDROID}

  {$IFDEF IOS} // Home btn handler for iOS
  if TPlatformServices.Current.SupportsPlatformService(StringToGUID('{F3AAF11A-1678-4CC6-A5BF-721A24A676FD}'),
      IInterface(AEService)) then
        AEService.SetApplicationEventHandler(AppEventHandler);
  {$ENDIF IOS}
end;

//handle AppEvents to detect Home btn pressed ( going to BG, disable sensors )
function TFormAirlinerAttitude.AppEventHandler(AAppEvent: TApplicationEvent;  AContext: TObject): Boolean;
var s:String;
begin
  if (AAppEvent =  TApplicationEvent.EnteredBackground) then  // Home btn
     begin
       fMagAccelFusion.StartStopSensors({bStart:} false );  //stop sensor feed
     end
  else if (AAppEvent = TApplicationEvent.BecameActive )  then
    begin   //returned from Home
       fMagAccelFusion.StartStopSensors({bStart:} true );   //restart sensor feed
    end;
  Result := True; // apparently this doesn't matter on iOS
end;

procedure TFormAirlinerAttitude.Form3DActivate(Sender: TObject);
begin
  {$IFDEF Android}  // request permissions to work
  FRequester.RequestPermissions([ cPermissionAccessCoarseLocation,   // location (gyro,aceler)
                                  cPermissionAccessFineLocation],
                                  cPermissionsBoatAttitude);     // commented out cPermissionAccessMockLocation
  //  On Android, sensors are started after permission is checked
  {$ENDIF Android}

  {$IFDEF IOS}
  fMagAccelFusion.StartStopSensors({bStart:} true );  //start sensor feed
  {$ENDIF IOS}
  // show all stuff that might be left invisible at design time
  dummyCameraGroup.Visible := true;
  // dummyBoatGroup.Visible := true;
  // modelBoat.Visible := true;
  // modelLiteSpi.Visible := true;
  // planeMainSail.Visible := true;

  dummyAirliner.Visible := true;
  layerDisplay.Visible  := true;
end;

{$IFDEF Android}      // Android requires permissions for things like sensors
procedure TFormAirlinerAttitude.PermissionsResultHandler(Sender: TObject; const ARequestCode: Integer; const AResults: TPermissionResults);
var LDeniedResults: TPermissionResults;
    LDeniedPermissions: string; i:integer;
begin
  case ARequestCode of  //Android permission request handler
    cPermissionsBoatAttitude:
    begin
      if AResults.AreAllGranted then  //all granted, start sensors (Android)
        begin
          fMagAccelFusion.StartStopSensors({bStart:} true );  //start sensor feed
        end
        else begin   // denied permissions
          LDeniedPermissions := '';
          LDeniedResults := AResults.DeniedResults;
          for I := 0 to LDeniedResults.Count - 1 do
            LDeniedPermissions := LDeniedPermissions + #13#10 + LDeniedResults[I].Permission;
          //ShowMessage('You denied permissions ' + LDeniedPermissions + ' I need those !');
        end;
    end;
  end;
end;
{$ENDIF Android}

//------------------------------------------
//       phone attitude axis ( Euler angles )
//          -Y     Z       altitude X up positive
//           |    /        heading  Y down positive
//           |   /         roll     Z positive into the screen
//       /=======\
//       |   | / |
//       |   |/  |
//       |   *---|--------- X
//       |       |
//       |   O   |
//       \-------/
//
//------------------------------------------

function normalize360(const a:Single):Single;
begin
  Result := a;
  while (Result<0) do Result := Result +360;
  while (Result>=360) do Result := Result -360;
end;

// handler for sensor fusion readings ( phone attitude chg )
procedure TFormAirlinerAttitude.FusionSensorHeadingAltitudeChanged(Sender:TObject);
var aAlt,aHead,aRoll:Single; s:String;  aSignal:integer;
  Q:TQuaternion3D;
  aSensorVec,tbVec,defVec:TVector3D;

begin
  aHead := fMagAccelFusion.fTCMagHeading;  // sensor fusion
  if (aHead=270) then aHead:=0;            // TESTE 270 means no magnetic readings ??

  aAlt  := fMagAccelFusion.fAltitude;
  aRoll := fMagAccelFusion.fRoll;

  s := 'Head:'+  Trim(Format('%5.0f°',[aHead]))+
       ' Ele:'+  Trim(Format('%5.0f°',[aAlt ]))+
       ' Roll:'+ Trim(Format('%5.0f°',[aRoll]));   // roll  -- az
  Label1.Text := s;                                // alt   -- roll

  labX.Text := Format('%5.0f°', [ tbX.Value ]);   // trackbars are used to add extra rotations
  labY.Text := Format('%5.0f°', [ tbY.Value ]);   // to sensor readings ( to "reset" object attitude )
  labZ.Text := Format('%5.0f°', [ tbZ.Value ]);
  // show GPS pos

  s := Format('%5.1f°', [fMagAccelFusion.fLocationLat])+' / '+Format('%5.1f°', [fMagAccelFusion.fLocationLon]);
  labAttitude.Text := s;

  // inverting angles ?        ( this is false and hiden for now )
  if cbInvert.IsChecked then aSignal := -1
    else aSignal := +1;                // camera attitude = phone attitude

  aSensorVec := TVector3D.Create( aAlt,aHead,aRoll  );                   //sensor reading
  tbVec      := TVector3D.Create(tbX.Value, tbY.Value, tbZ.Value );      //+ trackbars (0..360)
  defVec     := TVector3D.Create(fDefaultRotation.X,fDefaultRotation.Y,fDefaultRotation.Z);   // default=0
  aSensorVec := (aSensorVec+tbVec)*aSignal + defVec;    // sensor reading + trackbars + default rotation

  aSensorVec := -aSensorVec; ///invert test

  // was ToQuaternion({yaw:}aSensorVec.z,{pitch:}0 {aSensorVec.y},{roll:}aSensorVec.x, Q ); // axis order found by trial n error :( ?
  // it's more like:
  // make plane pitch = roll/2, so when it rolls we can see the plane side ( like some real plane, i suppose )
  ToQuaternion({roll:}aSensorVec.z,{pitch:} aSensorVec.z/2  {aSensorVec.y},{yaw:}aSensorVec.x, Q );

  dummyAirliner.SetMatrix(Q);   // rotate camera pointing to boat using quaternion

  dummySeaDisk.RotationAngle.Y := aSensorVec.Y;  //rotate compass disk
end;

end.
