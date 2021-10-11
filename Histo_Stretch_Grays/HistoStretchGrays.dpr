// efg, November 1998; updated May 1999

program HistoStretchGrays;

uses
  Forms,
  ScreenHistoStretchGrays in 'ScreenHistoStretchGrays.pas' {FormHistoStretchGrays},
  HistogramLibrary in 'HistogramLibrary.pas';

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TFormHistoStretchGrays, FormHistoStretchGrays);
  Application.Run;
end.
