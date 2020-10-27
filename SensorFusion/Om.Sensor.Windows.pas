unit Om.Sensor.Windows;   // derived from DW.Sensor.Android
// bring cross platform functionality to DW.Sensor ( from DelphiWorlds  )
// - code adapted by Omar ago19
// - Windows sensor simulation added ( since windows PCs have no sensors ( at least mine doesn't )  )

interface

uses
  System.Sensors, {TCustomMotionSensor,TCustomOrientationSensor}
  System.Math.Vectors, // TVector3D
  FMX.Types,           // TTimer
  DW.Sensor;

type
  TPlatformSensor = class(TCustomPlatformSensor)    // Windows
  private
  private
    FIsActive: Boolean;
    FSensorStart: TDateTime;
    FTimestamp: Int64;
    // for Windows, this is just a test environment w/ simulated outputs
    // iOS sensor code from CamSextant app
    //   fGyroSensor: TCustomMotionSensor;
    //   fMagSensor:  TCustomOrientationSensor;

    fTimer:TTimer;      // timer used to simulate random variations in simulation  sensors
    procedure TimerTick(Sender: TObject);  // use timer to get the sensor values. It would be better to receive sensor notifications, but I don't know how ???
  protected
    function  GetIsActive: Boolean; override;
    procedure SetIsActive(const Value: Boolean); override;
    procedure SetSensorType(const Value: TSensorType); override;
  public
    // simulation values
    fGyroAcceleration:TVector3d;  // simulated sensor readings
    fMagHeading:TVector3d;

    class function IsSensorTypeSupported(const ASensorType: TSensorType): Boolean;
  public
    constructor Create(const ASensor: TSensor); override;
    destructor  Destroy; override;
  end;

implementation

uses
  System.DateUtils, System.SysUtils;

{ TPlatformSensor }

constructor TPlatformSensor.Create(const ASensor: TSensor);
begin
  inherited;
  FIsActive    := false;
  FSensorStart := 0;
  FTimestamp   := 0;

  fTimer := TTimer.Create(nil);
  fTimer.Interval := 1000;
  fTimer.OnTimer := TimerTick;
  fTimer.Enabled := False;

  // set simulation init values
  fGyroAcceleration := Vector3d( 0.01, 0.95, 0.01 );
  fMagHeading       := Vector3d( 0.1 , 0.0 , 0.7 );    // head= ele= roll= more or less
end;

destructor TPlatformSensor.Destroy;
begin
  //FListener := nil;
  fTimer.Free;
  inherited;
end;

function TPlatformSensor.GetIsActive: Boolean;
begin
  Result := FIsActive;
end;

class function TPlatformSensor.IsSensorTypeSupported( const ASensorType: TSensorType): Boolean;
begin
  Result := ( ASensorType=Accelerometer ) or ( ASensorType= MagneticField);   // only these two supported at this time ( the ones I needed )
  //TODO: More sensors
end;

procedure doSmallVariationVector(var aVector:TVector3d);
var r:Single;
begin
  r := (Random(100)-50)/1000;         aVector.x := aVector.x*(1+r);     //10% variation
  r := (Random(100)-50)/1000;         aVector.y := aVector.y*(1+r);
  r := (Random(100)-50)/1000;         aVector.z := aVector.z*(1+r);
end;

// iOS sensor polled in a 100 ms timer
procedure TPlatformSensor.TimerTick(Sender:TObject);
var  aVx,aVy,aVz:double;
     LValues: TSensorValues;

     procedure _NotifyValues;
     begin
       SetLength(LValues, 3);
       LValues[0] := aVx;
       LValues[1] := aVy;
       LValues[2] := aVz;

       if (FTimestamp=0) then FSensorStart := Now;
       ValuesChanged(LValues, Now  );  // call sensor event
     end;

begin
  case FSensorType of
     Accelerometer:
       begin
         if fIsActive then
           begin
             doSmallVariationVector(fGyroAcceleration);
             aVx := fGyroAcceleration.X;   // iPhone acceleration vector is
             aVy := fGyroAcceleration.Y;   // negative in relation to Android
             aVz := fGyroAcceleration.Z;   // invert to make both work in the Android convention
             _NotifyValues;
           end;
       end;
     MagneticField:
       begin
         if fIsActive then
           begin
             doSmallVariationVector(fMagHeading);
             aVx := fMagHeading.X;    // mag field is the same for iOS and Android, it seems
             aVy := fMagHeading.Y;
             aVz := fMagHeading.Z;
             _NotifyValues;
           end;
       end;
     //TODO: Other sensors
  end;
end;

procedure TPlatformSensor.SetIsActive(const Value: Boolean);
var Sensor:TCustomSensor; aSensors:TSensorArray; aSensorManager: TSensorManager;
begin
  if Value = FIsActive then   Exit; // <======

  if Value then
    begin // get and activate the sensor manager
      fTimer.Enabled := true;
    end
    else begin  //setIsACTIVE(FALSE)
      fTimer.Enabled := false;
    end;
  FIsActive := Value;
end;


procedure TPlatformSensor.SetSensorType(const Value: TSensorType);
var
  LIsActive: Boolean;
begin
  if Value=FSensorType then
    Exit; // <======

  LIsActive := GetIsActive;
  SetIsActive(False);
  FSensorType := Value; //chg sensor type
  FTimestamp := 0;
  SetIsActive(LIsActive);
end;

end.

