unit hsstrings;

interface

resourcestring

  
  //////////////////////////////////////////////////////////////////////////////
  // Exception messages.
  //////////////////////////////////////////////////////////////////////////////


  SUpdateThreadWinit = 'The thread does not respond to the INIT command';  //ok
  SCompIsntADriver = 'The component is not a valid protocol driver';    //ok
  SthreadSuspended ='The thread is suspended?'; //ok
  ScannotBlinkWithItSelf = 'The animation zone can''t blink with it self!';  //ok
  SPLCMinPLCMaxMustBeDifferent = 'The properties PLCMin and PLCMax must be different!';  //ok
  SsysMinSysMaxMustBeDifferent = 'The properties SysMin and SysMax must be different!';  //ok
  SportNumberRangeError = 'The port value must be between 1 and 65535!'; //ok
  SDBConnectionRequired = 'You must be logged into the database!'; //ok
  SonlyPLCTagNumber = 'This driver supports only TPLCTagNumber. TPLCBlock and TPLCString aren''t supported!'; //ok
  STagAlreadyRegiteredWithThisDriver = 'This Tag already linked with this protocol driver!'; //ok
  SerrorInitializingWinsock = 'Error initializing the WinSock!';  //ok
  SoutOfBounds = 'Out of bounds!'; //ok
  SimpossibleToChangeWhenActive = 'Cannot change the communication port properties while it''s active!'; //ok
  SimpossibleToRemoveWhenBusy = 'Cannot remove the the connection while it''s being used!';      //ok
  SincrementMustBeGreaterThanZero = 'The increment must be a value greater than zero!'; //ok
  SinvalidInterface = 'Invalid interface!';  //ok
  SoutOfMemory = 'Message pool out of memory!';  //ok
  SinvalidMode = 'Invalid mode!';  //ok
  SsizeMustBe7or8 = 'The byte size can be 7 or 8 only!';
  SmaxMustBeGreaterThanMin = 'The maximum value must be greater than minimum!';
  SminMustBeLessThanMax = 'The minimum value must be less than maximum!'; //ok
  StheValueMustBeDifferentOfValueFalseProperty = 'The value must be different of the ValueFalse property!';
  StheValueMustBeDifferentOfValueTrueProperty = 'The value must be different of the ValueTrue property!';
  SonlyMySQL_SQLite_PostgresSupported = 'The supported databases are: MySQL, SQLite and PostgreSQL';  //ok
  SztBitcomparationValue1MustBeBetween0And31 = 'To use the ztBit selection criteria, the value of Value1 must be greater or equal than 0 and less or equal than 31!';
  SserialPortNotExist = 'Serial port does not exists!';
  SwithoutDBConnection = 'Without a database connection!';
  SonlyNumericTags = 'Only numeric tags are acceptable!';
  SuserTableCorrupted = 'Group members table does not exists!';
  SgroupsTableNotExist = 'Group table does not exists!';  //ok
  SusersTableNotExist = 'Users table does not exists!'; //ok
  StablesCorrupt = 'Database tables corrupted!';
  SinvalidTag = 'Invalid Tag!';
  SsizeMustBeAtLeastOne = 'The size must be at least 1!'; //ok
  SstringSizeOutOfBounds = 'String size out of bounds!';
  SinvalidType = 'Invalid type!'; //ok
  SinvalidValue = 'Invalid value!';
  SinvalidWinSockVersion = 'Wrong version of the Winsock. Requires version 2.0 or later!';
  SFaultGettingLastOSError = 'Error getting the error messege from operating system';
  SWithoutTag = 'WITHOUT TAG!';
  SEmpty      = 'Empty';
  SWithoutTagBuilder = 'The protocol driver does not support the Tag Builder tool';
  SInvalidTagNameInTagBuilder = 'Invalid name!';
  SWithoutAtLeastOneValidName = 'You must have at least on item with a valid name or the option "Count empty" must be checked!';
  SInvalidBlockName           = 'Invalid block name!';
  SCannotDestroyBecauseTagsStillManaged= 'The tag manager cannot be destroyed while it''s in use!';
  SCannotRebuildTagID = 'Cannot rebuild the tag identification (TagID).';
  SItemOutOfStructure = 'The item size exceeds the area of structure.';
  SGetP0 = 'Gets initial position';
  SGetP1 = 'Gets final position';
  SGotoP0 = 'Controls go to P0 position';
  SScanableNotSupported = 'The IScanableTagInterface Interface isn''t supported!';
  SCheckAtLeastOneVariable = 'Check at least one variable!';
  STheOwnerMustBeAProtocolDriver = 'The component owner of the update thread must be a protocol driver!';
  SYouMustHaveAtLeastOneStructureItem = 'You must have at least one structure item!';
  SWhyMapBitsFromOtherBits = 'Why to map bits from other bits?';
  SStartMustBeLessThanEndIndex = 'Start index must be less or equal than end index';
  SDoYouWantDeleteThisItem = 'Do you want to delete this item?';
  SDeleteTheItem = 'Delete item "';
  SDigitalInputInitialByte  = 'Initial byte of digital input';
  SDigitalOutputInitialByte = 'Initial byte of digital ouput';
  SFlagInitialAddress       = 'Initial address of Flags(M)';
  SInitialAddressInsideDB   = 'Start address inside of DB';
  SCounterInitialAddress    = 'Initial address of Counter';
  STimerInitialAddress      = 'Initial address of Timer';
  SSMInitialByte            = 'Initial address of SM';
  SAIWInitialAddress        = 'Initial address of AIW';
  SAQWInitialAddress        = 'Initial address of AQW';
  SPIWInitialAddress        = 'Initial address of PIW';
  SVInitialAddress          = 'Initial address of V';
  SRemoveaStructItemCalled  = 'Remove the structure item called "';

implementation

end.
 
