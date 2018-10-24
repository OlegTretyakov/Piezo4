unit MilandrRev8.Consts;

interface

const
C_INF_Min = 0;
C_INF_Mean = 31;
C_INF_Max = 63;

C_LIN_Min = 0;
C_LIN_Mean = 127;
C_LIN_Max = 255;

C_SCALE_Min = 0;
C_SCALE_Mean = 15;
C_SCALE_Max = 31;

C_CUB_Min = 0;
C_CUB_Mean = 7;
C_CUB_Max = 15;

C_FOUR_Min = 0;
C_FOUR_Mean = 7;
C_FOUR_Max = 15;

C_FIFTH_Min = 0;
C_FIFTH_Mean = 7;
C_FIFTH_Max = 15;

C_CDACC_Min = 0;
C_CDACC_Mean = 15;
C_CDACC_Max = 31;

C_CDACF_Min = 0;
C_CDACF_Mean = 3;
C_CDACF_Max = 7;

C_OFS_Min = 0;
C_OFS_Mean = 3;
C_OFS_Max = 7;

C_BitMinIndex = 0;
C_BitMaxIndex = 14;

C_CDACMaxBit = 8;


C_TEST_Index = 0;
C_INF_Index = 1;
C_LIN_Index = 2;
C_SCALE_Index = 3;
C_CUB_Index = 4;
C_FOUR_Index = 5;
C_FIFTH_Index = 6;
C_OFS_Index = 7;
C_CDACC_Index = 8;
C_CDACF_Index = 9;
C_DIV_Index = 10;
C_SC_Index = 11;
C_VTUNE_Index = 12;
C_AMPL_Index = 13;
C_FCH_Index = 14;

C_ChipName = 'Milandr.Rev8';
//This is s Milandr rev.8 GUID
C_GUID : TGUID = '{8FFC0287-0D45-4F0F-ABE3-4BD811E0695A}';


C_ValsDefs : array [C_BitMinIndex..C_BitMaxIndex] of smallint =
  (0, C_INF_Mean, C_LIN_Mean,
  C_SCALE_Mean, C_CUB_Mean, C_FOUR_Mean, C_FIFTH_Mean, 0, C_CDACC_Mean, C_CDACF_Mean,
  0, 0, 1, 0, 0);

C_ValsMax : array [C_BitMinIndex..C_BitMaxIndex] of smallint=(15, C_INF_Max, C_LIN_Max,
  C_SCALE_Max, C_CUB_Max, C_FOUR_Max, C_FIFTH_Max, C_OFS_Max, C_CDACC_Max, C_CDACF_Max,
  3, 1, 1, 1, 1);

C_ValsMeans : array [C_BitMinIndex..C_BitMaxIndex] of smallint =
  (7, C_INF_Mean, C_LIN_Mean,
  C_SCALE_Mean, C_CUB_Mean, C_FOUR_Mean, C_FIFTH_Mean, C_OFS_Mean, C_CDACC_Mean, C_CDACF_Mean,
  0, 0, 1, 0, 0);

C_ValsMin : array [C_BitMinIndex..C_BitMaxIndex] of smallint=(0, C_INF_Min, C_LIN_Min,
  C_SCALE_Min, C_CUB_Min, C_FOUR_Min, C_FIFTH_Min, C_OFS_Min, C_CDACC_Min, C_CDACF_Min,
  0, 0, 0, 0, 0);

C_RegNames : array[C_BitMinIndex..C_BitMaxIndex] of String =
      ('TEST','INF','LIN',
      'SCALE','CUB','FOUR','FIFTH','OFS','CDACC','CDACF',
      'DIV','SC','VTUNE','AMPL','FCH');
implementation

end.
