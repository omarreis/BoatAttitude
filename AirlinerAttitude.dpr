program AirlinerAttitude;

uses
  System.StartUpCopy,
  FMX.Forms,
  fFormAirlinerAttitude in 'fFormAirlinerAttitude.pas' {FormBoatAttitude};

{$R *.res}

begin
  Application.Initialize;
  Application.FormFactor.Orientations := [TFormOrientation.Portrait];
  Application.CreateForm(TFormAirlinerAttitude, FormAirlinerAttitude);
  Application.Run;
end.
