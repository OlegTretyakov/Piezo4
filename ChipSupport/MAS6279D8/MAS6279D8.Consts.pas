unit MAS6279D8.Consts;

interface


const
C_INF_Min = 0 ;
C_INF_Mean = 127;
C_INF_Max = 255;

C_LIN_Min = 0;
C_LIN_Mean = 254;
C_LIN_Max = 511;

C_SQ_Min = - 127;
C_SQ_Mean = 0;
C_SQ_Max = 127;

C_CUB_Min = 0;
C_CUB_Mean = 127;
C_CUB_Max = 255;

C_FIFTH_Min = -127;
C_FIFTH_Mean = 0;
C_FIFTH_Max = 127;

C_FOUR_Min = - 127;
C_FOUR_Mean = 0;
C_FOUR_Max = 127;

C_OFS_Min = 0;
C_OFS_Mean = 15;
C_OFS_Max = 31;

C_CDACC_Min = 0 ;
C_CDACC_Mean = 15;
C_CDACC_Max = 31;

C_CDACF_Min = 0 ;
C_CDACF_Mean = 15;
C_CDACF_Max = 31;

C_CDACMaxBit = 10;

//C_BufferSize = 10;
C_BitMinIndex = 0;
C_BitMaxIndex = 17;

C_ChipName = 'MAS6279.D8';
C_GUID : TGUID = '{A05E5E33-8586-43CA-AFC7-510899CDDBE2}';


C_ValsDef : array [C_BitMinIndex..C_BitMaxIndex] of smallint=
(0, 0, 0, C_INF_Mean, C_LIN_Mean, C_SQ_Mean, C_CUB_Mean, C_FOUR_Mean, C_FIFTH_Mean, C_OFS_Mean,
  0, C_CDACC_Mean, 0, C_CDACF_Mean, 0, 0, 1, 0);

C_ValsMax : array [C_BitMinIndex..C_BitMaxIndex] of smallint=
  (15, 1, 1, C_INF_Max, C_LIN_Max, C_SQ_Max, C_CUB_Max,
  C_FOUR_Max, C_FIFTH_Max, C_OFS_Max,
  1, C_CDACC_Max, 1, C_CDACF_Max, 1, 1, 1, 1);


C_ValsMeans : array [C_BitMinIndex..C_BitMaxIndex] of smallint=
(7, 0, 0, C_INF_Mean, C_LIN_Mean, C_SQ_Mean, C_CUB_Mean, C_FOUR_Mean, C_FIFTH_Mean, C_OFS_Mean,
  1, C_CDACC_Mean, 1, C_CDACF_Mean, 0, 0, 1, 0);


C_ValsMin : array [C_BitMinIndex..C_BitMaxIndex] of smallint=
(0, 0, 0, C_INF_Min, C_LIN_Min, C_SQ_Min, C_CUB_Min, C_FOUR_Min, C_FIFTH_Min, C_OFS_Min,
  0, C_CDACC_Min, 0, C_CDACF_Min, 0, 0, 0, 0);


C_RegNames : array[C_BitMinIndex..C_BitMaxIndex] of String =
      ('TEST', 'DA', 'CLK','INF','LIN','SQ','CUB','FOUR','FIFTH',
      'OFS','TCXO','CDACC','DRV','CDACF','DIV','SC','XOPD','NF');
implementation

end.
