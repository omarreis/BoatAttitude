program BoatAttitude;

uses
  System.StartUpCopy,
  FMX.Forms,
  fBoatAttitude2 in 'fBoatAttitude2.pas' {FormBoatAttitude};

{$R *.res}

begin
  Application.Initialize;
  Application.FormFactor.Orientations := [TFormOrientation.Portrait];
  Application.CreateForm(TFormBoatAttitude, FormBoatAttitude);
  Application.Run;
end.
