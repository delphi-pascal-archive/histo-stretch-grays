// Histogram and Image Statistics Library
// Assumes shades-of-gray images but stored in pf24bit bitmap to avoid
// working with palettes.
//
// Earl F. Glynn, April 1998.  Modified November 1998.

UNIT HistogramLibrary;

INTERFACE

  USES
    Windows,    // TRGBTriple
    Graphics;   // TCanvas

  TYPE
    THistoArray  = ARRAY[BYTE] OF INTEGER;

    TColorPlane  = (cpRed, cpGreen, cpBlue,   // RGB color space
                    cpValue,                  // V from HSV color space
                    cpIntensity);             // Intensity "plane"

    TRGBHistoArray =
      RECORD
        Red      :  THistoArray;
        Green    :  THistoArray;
        Blue     :  THistoArray;
        Intensity:  THistoArray;
      END;

    THistogram =
      CLASS(TObject)
        PRIVATE
          FHistogram:  THistoArray;
          FUNCTION GetCount:  INTEGER;

        PUBLIC
          CONSTRUCTOR Create;

          PROCEDURE Clear;
          PROCEDURE Draw (CONST Canvas:  TCanvas);
          PROCEDURE Increment(CONST index:  BYTE);
          FUNCTION  GetPercentileLevel(CONST percentile:  DOUBLE):  BYTE;
          PROCEDURE GetStatistics(VAR N:  INTEGER;
                                  VAR Minimum,Maximum:  BYTE;
                                  VAR Mode, Median:  BYTE;
                                  VAR Mean, StandardDeviation,
                                      Skewness, Kurtosis:  DOUBLE);
          PROPERTY Frequency:  THistoArray READ FHistogram WRITE FHistogram;
          PROPERTY Count    :  INTEGER     READ GetCount;
      END;

  PROCEDURE GetHistogram(CONST ColorPlane:  TColorPlane;
                         CONST Bitmap:  TBitmap;
                         VAR   Histogram:  THistogram);

IMPLEMENTATION

  USES
    Dialogs,                    // ShowMessage
    Math,                       // MaxIntValue, IntPwr
    SysUtils;                   // pByteArray, Exception

  CONST
    MaxPixelCount = 65536;
    NaN           = 0.0/0.0;   // Tricky way to define NaN

  TYPE
    EHistogramError  = CLASS(Exception);
    EStatisticsError = CLASS(Exception);

    // For pf24bit Scanlines
    pRGBTripleArray = ^TRGBTripleArray;
    TRGBTripleArray = ARRAY[0..MaxPixelCount-1] OF TRGBTriple;


  // == THistogram ======================================================

  // This Histogram class is specialized for image processing applications.
  // The frequency distribution is assumed to be for values ONLY in the
  // range 0..255.

  FUNCTION THistogram.GetCount:  INTEGER;
    VAR
      i:  BYTE;
  BEGIN
    RESULT := 0;
    FOR i := Low(THistoArray) TO High(THistoArray) DO
      INC(RESULT, FHistogram[i])
  END {GetCount};


  CONSTRUCTOR THistogram.Create;
  BEGIN
    Clear
  END {Create};


  PROCEDURE THistogram.Clear;
    VAR
      i:  BYTE;
  BEGIN
    FOR i :=  Low(THistoArray) TO High(THistoArray) DO
      FHistogram[i] := 0
  END {Clear};



  FUNCTION  THistogram.GetPercentileLevel(CONST percentile:  DOUBLE):  BYTE;
    VAR
      Continue          :  BOOLEAN;
      CumulativeCount   :  INTEGER;
      CumulativeFraction:  DOUBLE;
      Level             :  BYTE;
      N                 :  INTEGER;
      Target            :  DOUBLE;
  BEGIN
    Target := percentile / 100.0;  // Target is fraction, not percentage
    N := GetCount;
    Level := Low(THistoArray);
    CumulativeCount  := 0;

    Continue := TRUE;
    WHILE Continue DO
    BEGIN
      CumulativeCount    := CumulativeCount + FHistogram[Level];
      CumulativeFraction := CumulativeCount / N;
      Continue := (CumulativeFraction < Target) AND (Level < High(THistoArray)-1);
      IF   Continue
      THEN INC (Level)
    END;

    RESULT := Level
  END {GetPercentileLevel};


  PROCEDURE THistogram.GetStatistics(VAR N:  INTEGER;
                                     VAR Minimum,Maximum:  BYTE;
                                     VAR Mode, Median:  BYTE;
                                     VAR Mean, StandardDeviation,
                                         Skewness, Kurtosis:  DOUBLE);
    VAR
      Cumulative  :  INTEGER;
      i           :  BYTE;
      MaxFrequency:  INTEGER;
      M2          :  EXTENDED;
      M3          :  EXTENDED;
      M3Sum       :  EXTENDED;
      M4          :  EXTENDED;
      M4Sum       :  EXTENDED;
      x           :  EXTENDED;
      xSum        :  EXTENDED;  // Use floats to avoid integer overflow
      xSqrSum     :  EXTENDED;
  BEGIN
    N := 0;

    Minimum := 0;
    WHILE (FHistogram[Minimum] = 0) AND (Minimum < 255)
    DO  INC(Minimum);

    Maximum := 255;
    WHILE (FHistogram[Maximum] = 0) AND (Maximum > 0)
    DO  DEC(Maximum);

    // Mode is value with highest frequency.
    // For now, don't worry about a "tie".
    Mode := Minimum;

    MaxFrequency := FHistogram[Minimum];
    FOR i := Minimum {+1} TO Maximum DO    // Not clear why "+1" here causes
    BEGIN                                  // runtime problem
      IF  FHistogram[i] > MaxFrequency
      THEN BEGIN
        Mode := i;
        MaxFrequency := FHistogram[i]
      END
    END;

    // Calculate Mean and Standard Deviation
    xSum    := 0.0;
    xSqrSum := 0.0;
    FOR i := Minimum TO Maximum DO
    BEGIN
      INC(N, FHistogram[i]);
      x := i;
      xSum    := xSum    + FHistogram[i]*x;
      xSqrSum := xSqrSum + FHistogram[i]*SQR(x)
    END;

    IF   N = 0
    THEN Mean := NaN
    ELSE Mean := xSum / N;

    IF   N < 2
    THEN BEGIN
      StandardDeviation := NaN;
      Skewness := 0.0;
      Kurtosis := 0.0;
    END
    ELSE BEGIN
      StandardDeviation := SQRT( (xSqrSum - N*SQR(Mean)) / (N-1) );

      // Standard Deviation is related to moment M2
      M2 := SQR(StandardDeviation) * (N-1) / N;

      // Calculate third and fourth moments
      M3Sum := 0.0;
      M4Sum := 0.0;
      FOR i := Minimum TO Maximum DO
      BEGIN
        x := i;

        M3Sum := M3Sum + FHistogram[i]*IntPower(x - Mean, 3);
        M4Sum := M4Sum + FHistogram[i]*IntPower(x - Mean, 4);
      END;

      M3 := M3Sum / N;
      M4 := M4Sum / N;

      IF   M2 = 0.0
      THEN BEGIN
        Skewness := NaN;
        Kurtosis := 0.0
      END
      ELSE BEGIN
        Skewness := M3 / Power(M2, 1.5);
        Kurtosis := M4 / SQR(M2)
      END
    END;

    // Median is value with half of values above and below.
    Cumulative := 0;
    i := Minimum;
    WHILE (Cumulative < N DIV 2) AND (i < 255) DO
    BEGIN
      INC(Cumulative, FHistogram[i]);
      IF   Cumulative < N DIV 2     // fix for when all 0s
      THEN INC(i)
    END;
    Median := i;

  END {GetStatistics};


  PROCEDURE THistogram.Increment(CONST index:  BYTE);
  BEGIN
    INC(FHistogram[index])
  END {Increment};


  // Draw Histogram on specified Canvas, assumed to have
  // a width that is a multiple of 256 (for best results).
  PROCEDURE THistogram.Draw(CONST Canvas:  TCanvas);
    CONST
      MajorTickSize = 8;

    VAR
      BarLength:  INTEGER;
      Delta    :  INTEGER;
      Color    :  TColor;
      factor   :  DOUBLE;
      i        :  INTEGER;
      index    :  INTEGER;
      j        :  INTEGER;
      Height   :  INTEGER;
      MaxValue :  INTEGER;
      Width    :  INTEGER;
  BEGIN
    Height   := Canvas.ClipRect.Bottom;
    Width    := Canvas.ClipRect.Right;

    MaxValue := MaxIntValue(SELF.Frequency);
    factor := Width / 256;

    // For now only paint on a canvas exactly 256 pixels wide.  If
    // MaxValue is zero, array was not filled in correctly and is ignored.
    IF   MaxValue > 0
    THEN BEGIN
      FOR i := 0 TO Width-1 DO
      BEGIN
        // In general, Width should be multiple of 255 + 1
        index := ROUND(i / factor);
        index := MinIntValue( [255, index] );

        Color := RGB(index, index, index);
        Canvas.Pen.Color := Color;
        BarLength := ROUND(Height*SELF.Frequency[index] / MaxValue);
        Canvas.MoveTo(i, Height-1);
        Canvas.LineTo(i, Height-1-BarLength)
      END;

      Canvas.Pen.Color := clRed;
      // Vertical Lines for visual estimation
      FOR i := 0 TO 25 DO
      BEGIN
        Canvas.MoveTo(Round(10*i*factor), Height-1);
        IF   i MOD 5 = 0
        THEN Delta := MajorTickSize
        ELSE Delta := MajorTickSize DIV 2;
        Canvas.LineTo(Round(10*i*factor), Height-1-Delta);
      END;

      // Horizontal Lines
      FOR j := 1 TO 4 DO
      BEGIN
        Canvas.MoveTo(      0, j*Height DIV 5);
        Canvas.LineTo(Width-1, j*Height DIV 5);
      END;

      Canvas.Brush.Style := bsClear;
      Canvas.Font.Color := clRed;
      Canvas.TextOut(2,2, 'Max = ' + IntToStr(MaxValue));
      Canvas.TextOut(2, Height-Canvas.TextHeight('X') - MajorTickSize, '0');
      Canvas.TextOut(Width-Canvas.TextWidth('250 '),
                     Height-Canvas.TextHeight('X') - MajorTickSize, '250')
    END
  END {DrawHistogram};


  PROCEDURE GetHistogram(CONST ColorPlane:  TColorPlane;
                         CONST Bitmap    :  TBitmap;
                         VAR Histogram   :  THistogram);
    VAR
      i    :  INTEGER;
      index:  INTEGER;
      j    :  INTEGER;
      Row  :  pRGBTripleArray;
  BEGIN
    IF   (Bitmap.PixelFormat <> pf24bit)
    THEN RAISE EHistogramError.Create('GetHistogram:  ' +
               'Bitmap must be 24-bit.');

    Histogram.Clear;

    // Step through each row of image.
    FOR j := Bitmap.Height-1 DOWNTO 0 DO
    BEGIN
      Row  := Bitmap.Scanline[j];

      FOR i := Bitmap.Width-1 DOWNTO 0 DO
      BEGIN
        CASE ColorPlane OF
          // RGB Color Space
          cpRed  :  index := Row[i].rgbtRed;
          cpGreen:  index := Row[i].rgbtGreen;
          cpBlue :  index := Row[i].rgbtBlue;

          // V from HSV Color Space
          cpValue:  WITH Row[i] DO
                      index := MaxIntValue( [rgbtRed, rgbtGreen, rgbtBlue] );

          cpIntensity:  WITH Row[i] DO
                          index := (rgbtRed + rgbtGreen + rgbtBlue) DIV 3
          ELSE
            index := 0;   // should never happen; avoid compiler warning
        END;

        Histogram.Increment(index)
      END

    END

  END {GetHistogram};

END.
