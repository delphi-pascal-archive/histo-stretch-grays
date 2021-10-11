// efg, November 1998; updated May 1999, August 2002, June 2003

unit ScreenHistoStretchGrays;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls,
  ExtDlgs,
  ComCtrls,
  Mask,
  Spin;

type
  TFormHistoStretchGrays = class(TForm)
    ButtonReadImage: TButton;
    ButtonSave: TButton;
    OpenPictureDialog: TOpenPictureDialog;
    CheckBoxStretch: TCheckBox;
    ImageOriginal: TImage;
    LabelStatsKey: TLabel;
    LabelOriginalStats: TLabel;
    ImageHistogramOriginal: TImage;
    Label2: TLabel;
    LabelPixels: TLabel;
    LabelColors: TLabel;
    ImageHistoStretched: TImage;
    ImageHistogramStretched: TImage;
    Label3: TLabel;
    LabelStretchedStats: TLabel;
    LabelRightPercent: TLabel;
    SpinButtonTail: TSpinButton;
    MaskEditTail: TMaskEdit;
    LabelPercent: TLabel;
    ImageArrow: TImage;
    SpinButtonFactor: TSpinButton;
    MaskEditFactor: TMaskEdit;
    LabelFactor: TLabel;
    ButtonWriteImage: TButton;
    SavePictureDialog: TSavePictureDialog;
    ButtonCopyToClipboard: TButton;

    procedure ButtonReadImageClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure CheckBoxStretchClick(Sender: TObject);
    procedure SpinButtonTailUpClick(Sender: TObject);
    procedure SpinButtonTailDownClick(Sender: TObject);
    procedure SpinButtonFactorUpClick(Sender: TObject);
    procedure SpinButtonFactorDownClick(Sender: TObject);
    procedure MaskEditTailChange(Sender: TObject);
    procedure MaskEditFactorChange(Sender: TObject);
    procedure ButtonWriteImageClick(Sender: TObject);
    procedure ButtonCopyToClipboardClick(Sender: TObject);
  private
    OriginalRangeLeft:  INTEGER;
    ScaleFactor      :  DOUBLE;
    StretchFactor    :  DOUBLE;
    TailPercentLeft  :  DOUBLE;
    TailPercentRight :  DOUBLE;
    ShowDesignHeight :  INTEGER;
    ShowDesignWidth  :  INTEGER;

    PROCEDURE UpdateDisplay;
    PROCEDURE UpdateTailPercentage(CONST sign:  INTEGER);
    PROCEDURE UpdateFactor(CONST sign:  INTEGER);

    FUNCTION CreateHistoStretchBitmap:  TBitmap;
  public
    OriginalBitmap:  TBitmap;
  end;

var
  FormHistoStretchGrays: TFormHistoStretchGrays;

implementation
{$R *.DFM}

  USES
{$IFDEF GIF}
    GIFImage,         // TGIFImage (by Anders Melander)
{$ENDIF}
    ClipBrd,          // Clipboard
    JPEG,             // JPEG support at runtime
    HistogramLibrary, // THistogram
    Math;             // MinIntValue, MaxIntValue

  CONST
    MaxPixelCount = 65536;

  TYPE
    // For pf24bit Scanlines
    pRGBTripleArray = ^TRGBTripleArray;
    TRGBTripleArray = ARRAY[0..MaxPixelCount-1] OF TRGBTriple;


  //==  Plural  ==========================================================

  FUNCTION Plural(CONST n:  LongInt; CONST singularform,pluralform:  STRING):  STRING;
  BEGIN  // function similar to one on p. 314, Byte, December 1988
    IF   n = 1
    THEN RESULT := singularform
    ELSE
      IF   pluralform = ''
      THEN RESULT := singularform + 's'
      ELSE RESULT := pluralform
  END {Plural};


  //==  RGBTripleToY  ====================================================

  // Y (in YIQ) = 0.299R + 0.587G + 0.114B, which can be performed in
  // integer arithmetic as Y = (77R + 150G + 29B) DIV 256
  // See [Foley96, pp. 589-590]
  FUNCTION  RGBTripleToY (CONST RGB:  TRGBTriple):  INTEGER;
  BEGIN
    WITH RGB DO
      RESULT := INTEGER(77*rgbtRed + 150*rgbtGreen + 29*rgbtBlue) SHR 8
  END {RGBtoY};


  //==  CountColors  =====================================================

  // Count number of unique R-G-B triples in a pf24bit Bitmap.
  //
  // Use 2D array of TBits objects -- when (R,G) combination occurs
  // for the first time, create 256-bit array of bits in blue dimension.
  // So, overall this is a fairly sparse matrix for most pictures.
  // Tested with pictures created with a known number of colors, including
  // a specially constructed image with 1024*1024 = 1,048,576 colors.
  //
  // efg, October 1998.
  FUNCTION CountColors(CONST Bitmap:  TBitmap):  INTEGER;
    VAR
      Flags:  ARRAY[BYTE, BYTE] OF TBits;
      i    :  INTEGER;
      j    :  INTEGER;
      k    :  INTEGER;
      rowIn:  pRGBTripleArray;
  BEGIN
    // Be sure bitmap is 24-bits/pixel
    ASSERT (Bitmap.PixelFormat = pf24Bit);

    // Clear 2D array of TBits objects
    FOR j := 0 TO 255 DO
      FOR i := 0 TO 255 DO
        Flags[i,j] := NIL;

    // Step through each scanline of image
    FOR j := 0 TO Bitmap.Height-1 DO
    BEGIN
      rowIn  := Bitmap.Scanline[j];
      FOR i := 0 TO Bitmap.Width-1 DO
      BEGIN
        WITH rowIn[i] DO
        BEGIN

          IF   NOT Assigned(Flags[rgbtRed, rgbtGreen])
          THEN BEGIN
            // Create 3D column when needed
            Flags[rgbtRed, rgbtGreen] := TBits.Create;
            Flags[rgbtRed, rgbtGreen].Size := 256;
          END;

          // Mark this R-G-B triple
          Flags[rgbtRed,rgbtGreen].Bits[rgbtBlue] := TRUE
        END
      END
    END;

    RESULT := 0;
    // Count and Free TBits objects
    FOR j := 0 TO 255 DO
    BEGIN
      FOR i := 0 TO 255 DO
      BEGIN

        IF   Assigned(Flags[i,j])
        THEN BEGIN
          FOR k := 0 TO 255 DO
            IF   Flags[i,j].Bits[k]
            THEN INC(RESULT);
          Flags[i,j].Free
        END

      END
    END

  END {CountColors};

// ========================================================================

procedure TFormHistoStretchGrays.ButtonReadImageClick(Sender: TObject);
  VAR
    ColorCount:  INTEGER;
    Converted :  BOOLEAN;
    GrayCount :  INTEGER;
    i         :  INTEGER;
    Intensity :  INTEGER;
    j         :  INTEGER;
    Picture   :  TPicture;
    row       :  pRGBTripleARray;
begin
  IF   OpenPictureDialog.Execute
  THEN BEGIN
    Screen.Cursor := crHourGlass;
    TRY
      IF   Assigned(OriginalBitmap)
      THEN OriginalBitmap.Free;

      OriginalBitmap := TBitmap.Create;

      // Use polymorphic TPicture to load any registered file type
      Picture := TPicture.Create;
      TRY
        Picture.LoadFromFile(OpenPictureDialog.Filename);

        // Try converting into bitmap
        TRY
          OriginalBitmap.Assign(Picture.Graphic);
        EXCEPT
          OriginalBitmap.Width  := Picture.Graphic.Width;
          OriginalBitmap.Height := Picture.Graphic.Height;
          OriginalBitmap.PixelFormat := pf24bit;
          OriginalBitmap.Canvas.Draw(0,0, Picture.Graphic)
        END;

      FINALLY
        Picture.Free
      END;

      // Force Pixelformat to pf24bit so we have R,G,B components to work with.
      // Surprisingly, after establishing a PixelFormat of pf24bit, setting
      // BitmapOriginal.Monochrome := TRUE, results in a change of PixelFormat
      // to pfDevice, which is not convenient for image processing. So,
      // gray scale conversion is forced below.
      OriginalBitmap.PixelFormat := pf24bit;

      FormHistoStretchGrays.Caption := 'Histo Stretch Grays: "' +
                                        ExtractFileName(OpenPictureDialog.Filename)+'"';
      ColorCount := CountColors(OriginalBitmap);

      // Convert any/all "color" pixels to intensity values-- but if original
      // image already in shades of gray, no conversion will occur.
      Converted := FALSE;
      FOR j := 0 TO OriginalBitmap.Height-1 DO
      BEGIN
        row := OriginalBitmap.Scanline[j];
        FOR i := 0 TO OriginalBitmap.Width-1 DO
        BEGIN
          WITH row[i] DO
          BEGIN  // Force R = G = B
            IF   (rgbtRed <> rgbtGreen) OR (rgbtRed <> rgbtBlue)
            THEN BEGIN
              Converted := TRUE;

              // Use the "Y" from "YIQ" coordinates -- this is the black/white
              // image like seen on old black/white TV sets.
              Intensity := RGBTripleToY(row[i]);

              rgbtRed   := Intensity;
              rgbtGreen := Intensity;
              rgbtBlue  := Intensity
            END
          END
        END
      END;

      GrayCount := CountColors(OriginalBitmap);
      IF   Converted
      THEN LabelColors.Caption := FormatFloat(',##0', ColorCount) +
                                  Plural(ColorCount, ' color', '') +
                                  ' reduced to ' +
                                  IntToStr(GrayCount)   +
                                  Plural(GrayCount, ' shade', '') +
                                  ' of gray'
      ELSE LabelColors.Caption := IntToStr(GrayCount)   +
                                  Plural(GrayCount, ' shade', '') +
                                  ' of gray';

      // OK to show image now
      ImageOriginal.Picture.Graphic := OriginalBitmap;

      CheckBoxStretch.Checked := (OriginalBitmap.Width  > ShowDesignWidth) OR
                                 (OriginalBitmap.Height > ShowDesignHeight);

      UpdateDisplay
    FINALLY
      Screen.Cursor := crDefault
    END;

    ButtonWriteImage.Enabled      := TRUE;
    ButtonCopyToClipboard.Enabled := TRUE;
  END
end;


procedure TFormHistoStretchGrays.FormCreate(Sender: TObject);
begin
  // Make sure ICO, WMF, GIF, JPG, GIF, etc. are in list
  OpenPictureDialog.Filter := GraphicFilter(TGraphic);

  ShowDesignWidth  := ImageOriginal.Width;
  ShowDesignHeight := ImageOriginal.Height;

  TailPercentLeft  :=  1.0;
  TailPercentRight := 99.0;
  StretchFactor    :=  1.0;

  ImageOriginal.Picture.Graphic := NIL;
  FormHistoStretchGrays.DoubleBuffered := TRUE
end;


PROCEDURE TFormHistoStretchGrays.UpdateDisplay;
  VAR
    OriginalHistogram :  THistogram;
    OriginalRangeRight:  INTEGER;
    OriginalRange     :  INTEGER;
    StretchedBitmap   :  TBitmap;
    StretchedHistogram:  THistogram;
    StretchedRange    :  INTEGER;

  PROCEDURE ShowHistogram(CONST Histogram:  THistogram;
                          CONST Image:  TImage;
                          CONST LabelStats:  TLabel);
    CONST
      clSkyBlue    = TColor($F0CAA6);   // RGB:  166 202 240
    VAR
      Kurtosis         :  DOUBLE;
      Maximum          :  BYTE;
      Mean             :  DOUBLE;
      Median           :  BYTE;
      Minimum          :  BYTE;
      Mode             :  BYTE;
      N                :  INTEGER;
      Skewness         :  DOUBLE;
      StandardDeviation:  DOUBLE;
  BEGIN
    Image.Canvas.Brush.Color := clSkyBlue;
    Image.Canvas.FillRect(Rect(0, 0, Image.Width, Image.Height));

    Histogram.Draw(Image.Canvas);
    Histogram.GetStatistics(N, Minimum, Maximum,
       Mode, Median,
       Mean, StandardDeviation,
       Skewness, Kurtosis);

    LabelPixels.Caption :=  'Pixels: ' + FormatFloat(',##0', N);

    LabelStats.Caption := 'Statistics:  ' +
      Format('%d / %d / %d / %d / %.1f / %.1f / %.1f',
      [Minimum,Maximum,Mode,Median,Mean,StandardDeviation, Kurtosis-3.0])
  END {UpdateHistogram};

BEGIN
  Screen.Cursor := crHourGlass;
  TRY

    // Update Display of Histograms.  Some work will be redone below to create
    // optimal Histostretch parameters.

    OriginalHistogram := THistogram.Create;
    TRY
      // Ask for cpRed, but cpGreen or cpBlue will give identical results
      // since R = G = B for shades of gray
      GetHistogram(cpRed, OriginalBitmap, OriginalHistogram);
      // Show Original Histogram
      ShowHistogram(OriginalHistogram, ImageHistogramOriginal,  LabelOriginalStats);

      OriginalRangeLeft  := OriginalHistogram.GetPercentileLevel(TailPercentLeft);
      OriginalRangeRight := OriginalHistogram.GetPercentileLevel(TailPercentRight);
      OriginalRange      := OriginalRangeRight - OriginalRangeLeft;

      // Normally, StretchFactor is 1.00 and RangeStretched = 255 (full width)
      StretchedRange := OriginalRange + ROUND( StretchFactor*(255 - OriginalRange) );

      IF   OriginalRange = 0.0
      THEN ScaleFactor := 1.0
      ELSE ScaleFactor := StretchedRange / OriginalRange;

      StretchedBitmap := CreateHistoStretchBitmap;
      TRY
        ImageHistoStretched.Picture.Graphic := StretchedBitmap;
        StretchedHistogram := THistogram.Create;
        TRY
          GetHistogram(cpRed, StretchedBitmap, StretchedHistogram);
          ShowHistogram(StretchedHistogram, ImageHistogramStretched,  LabelStretchedStats);
        FINALLY
          StretchedHistogram.Free
        END

      FINALLY
        StretchedBitmap.Free
      END
    FINALLY
      OriginalHistogram.Free
    END

  FINALLY
    Screen.Cursor := crDefault
  END
END {UpdateDisplay};


procedure TFormHistoStretchGrays.FormDestroy(Sender: TObject);
begin
  OriginalBitmap.Free
end;


procedure TFormHistoStretchGrays.CheckBoxStretchClick(Sender: TObject);
begin
  ImageOriginal.Stretch := CheckBoxStretch.Checked;
  ImageHistoStretched.Stretch := CheckBoxStretch.Checked;
end;


PROCEDURE TFormHistoStretchGrays.UpdateTailPercentage(CONST sign:  INTEGER);
  VAR
    TenthPercent:  INTEGER;
BEGIN
  TRY
    TenthPercent := ROUND( 10* StrToFloat(MaskEditTail.Text) );
  EXCEPT
    ON EConvertError DO TenthPercent := 0;  // 0.0 %
  END;

  TenthPercent := TenthPercent + sign;
  TenthPercent := MinIntValue( [MaxIntValue([0, TenthPercent]), 99 ]);

  TailPercentLeft  := TenthPercent/10.0;
  TailPercentRight := 100.0 - TenthPercent/10.0;

  MaskEditTail.Text :=  FormatFloat('0.0', TailPercentLeft);

  LabelRightPercent.Caption := FormatFloat('##0.0', TailPercentRight) + '%';

  UpdateDisplay
END {UpdateTailPercentage};


procedure TFormHistoStretchGrays.SpinButtonTailUpClick(Sender: TObject);
begin
  UpdateTailPercentage(+1)
end;


procedure TFormHistoStretchGrays.SpinButtonTailDownClick(Sender: TObject);
begin
  UpdateTailPercentage(-1)
end;


procedure TFormHistoStretchGrays.MaskEditTailChange(Sender: TObject);
begin
  UpdateTailPercentage(0)
end;


PROCEDURE TFormHistoStretchGrays.UpdateFactor(CONST sign:  INTEGER);
  VAR
    Hundredth:  INTEGER;
BEGIN
  TRY
    Hundredth := ROUND( 100* StrToFloat(MaskEditFactor.Text) );
  EXCEPT
    ON EConvertError DO Hundredth := 100;  // 1.00
  END;

  Hundredth := Hundredth + 5*sign;
  Hundredth := MinIntValue( [MaxIntValue([0, Hundredth]), 100 ]);
  StretchFactor := Hundredth/100.0;

  MaskEditFactor.Text :=  FormatFloat('0.00', StretchFactor);

  UpdateDisplay
END {UpdateFactor};


// Intended to be used like this:
//  Bitmap := CreateHistoStretchBitmap;
//  TRY
//  ...
//  FINALLY
//    Bitmap.Free
//  END;
FUNCTION TFormHistoStretchGrays.CreateHistoStretchBitmap:  TBitmap;
  VAR
    i,j               :  INTEGER;
    Intensity         :  INTEGER;
    OriginalRow       :  pRGBTripleARray;
    StretchedRow      :  pRGBTripleArray;
begin
  // Create stretched bitmap
  RESULT := TBitmap.Create;

  RESULT.Width  := OriginalBitmap.Width;
  RESULT.Height := OriginalBitmap.Height;
  RESULT.PixelFormat := pf24bit;

  FOR j := 0 TO OriginalBitmap.Height-1 DO
  BEGIN
    OriginalRow  := OriginalBitmap.Scanline[j];
    StretchedRow := RESULT.Scanline[j];
    FOR i := 0 TO OriginalBitmap.Width-1 DO
    BEGIN
      // Use the R component, but R = G = B for shades of gray
      Intensity := OriginalRow[i].rgbtRed;

      // Calculate new intensity level
      Intensity := ROUND(ScaleFactor*(Intensity - OriginalRangeLeft) );
      IF   Intensity < 0
      THEN Intensity := 0
      ELSE
        IF   Intensity > 255
        THEN Intensity := 255;

      WITH StretchedRow[i] DO
      BEGIN
        rgbtRed   := Intensity;
        rgbtGreen := Intensity;
        rgbtBlue  := Intensity
      END

    END
  END
END {CreateHistoStretchBitamp};


procedure TFormHistoStretchGrays.SpinButtonFactorUpClick(Sender: TObject);
begin
  UpdateFactor (+1)
end;


procedure TFormHistoStretchGrays.SpinButtonFactorDownClick(Sender: TObject);
begin
  UpdateFactor (-1)
end;


procedure TFormHistoStretchGrays.MaskEditFactorChange(Sender: TObject);
begin
  UpdateFactor (0)
end;


// efg, August 2002
// The original program didn't write the histostretched bitmap to a file, since
// I was too lazy to create a pf8bit bitmap, with a shades of gray palette,
// instead of working with the pf24bit bitmap.  I finally gave in to an
// E-mail, but forgot to put online until June 2003.

procedure TFormHistoStretchGrays.ButtonWriteImageClick(Sender: TObject);
  VAR
    StretchedBitmap   :  TBitmap;
begin
  IF   SavePictureDialog.Execute
  THEN BEGIN
    StretchedBitmap := CreateHistoStretchBitmap;
    TRY
      StretchedBitmap := CreateHistoStretchBitmap;
      StretchedBitmap.SaveToFile(SavePictureDialog.Filename);
    FINALLY
      StretchedBitmap.Free
    END
  END
end;

procedure TFormHistoStretchGrays.ButtonCopyToClipboardClick(
  Sender: TObject);
  VAR
    StretchedBitmap   :  TBitmap;
begin
  StretchedBitmap := CreateHistoStretchBitmap;
  TRY
    Clipboard.Assign(StretchedBitmap);
  FINALLY
    StretchedBitmap.Free
  END
end;

end.
