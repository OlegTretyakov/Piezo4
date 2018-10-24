unit commtypes;

interface

uses Classes;

type

  {:
  Sequence of bytes.
  @seealso(TCommPortDriver)
  @seealso(TIOPacket)
  }

  //BYTES = array of Byte;
  TByteArray = array of Byte;
  //pByteArray = ^TByteArray;
  TIOBuffer = array [0..260] of Byte;

  {:
  @name define the commands and their sequence of execution.

  @value iocNone Does nothing;
  @value iocRead Executes a read;
  @value iocReadWrite Executes a read command and after this a write command;
  @value iocWrite Executes a write command;
  @value iocWriteRead Executes a write command and after this a read command;

  @seealso(TCommPortDriver)
  @seealso(TCommPortDriver.IOCommandASync)
  @seealso(TCommPortDriver.IOCommandSync)
  @seealso(TIOPacket)
  }
  TIOCommand = (iocNone, iocRead, iocReadWrite, iocWrite, iocWriteRead);
  TIOAction = (ioaRead, ioaWrite);

  {:
  @name define the results of an I/O request.

  @value iorOk The request was done with successfull.
  @value iorTimeOut The request has a timeout.
  @value iorNotReady The communication port isn't ready yet. Example: communication port closed.
  @value iorNone The command was not processed;
  @value iorPortError A fault occurred while processing the I/O command.
  @seealso(TIOPacket)
  }
  TIOResult  = (iorOK, iorTimeOut, iorNotReady, iorNone, iorPortError);

  {:
  Return the results of an I/O request.

  @member PacketID Request identification.
  @member WriteIOResult Result of a write command, if exists. If not exists, return iorNone.
  @member ToWrite Number of @noAutoLink(bytes) to write.
  @member Written Number of @noAutoLink(bytes) written.
  @member WriteRetries Number of retries to write ToWrite @noAutoLink(bytes).
  @member BufferToWrite Sequence of @noAutoLink(bytes) to write. @bold(Must have at least ToWrite @noAutoLink(bytes) of length).
  @member DelayBetweenCommand Delay in milliseconds between the commands of Read and Write.
  @member ReadIOResult Result of a read command, if exists. If not exists, return iorNone.
  @member ToRead Number of @noAutoLink(bytes) to read.
  @member Received Number of @noAutoLink(bytes) received.
  @member ReadRetries Number of retries to read ToRead @noAutoLink(bytes).
  @member BufferToRead Buffer that stores @noAutoLink(bytes) received. Their length is adjusted to ToRead.

  @seealso(TCommPortDriver)
  @seealso(TCommPortDriver.IOCommandASync)
  @seealso(TCommPortDriver.IOCommandSync)
  }
  TIOPacket = record
    BufferToWrite:TIOBuffer;
    BufferToRead:TIOBuffer;
    ToWriteCount,
    WrittenCount,
    WriteRetries,
    DelayBetweenCommand,
    ToReadCount,
    ReceivedCount,
    ReadRetries:Cardinal;
    WriteIOResult:TIOResult;
    ReadIOResult:TIOResult;
    WriteStart,
    WriteFinish,
    ReadStart,
    ReadFinish : TDateTime;
  end;

  {:
  Pointer to a TIOPacket record.
  @seealso(TIOPacket)
  }
  pIOPacket = ^TIOPacket;

  {:
  Defines the callback procedure to return the results of a I/O command done by
  TCommPortDriver.IOCommandSync. The result is returned by the Result variable.

  @seealso(TCommPortDriver)
  @seealso(TIOPacket)
  @seealso(TCommPortDriver.IOCommandASync)
  @seealso(TCommPortDriver.IOCommandSync)
  }

  //: @exclude
  {$IFDEF FPC}
  TPSThreadID = TThreadID;
  {$ELSE}
  TPSThreadID = THandle;
  {$ENDIF}

  {$IFDEF PORTUGUES}
  //: Procedimento para sinalizar um evento de erro na porta
  {$ELSE}
  //: Defines a method called when a communication error occurs.
  {$ENDIF}
  TCommPortErrorEvent = procedure(const APort : TObject; Error:TIOResult) of object;
  //: @exclude
  //PCommPortErrorEvent = ^TCommPortErrorEvent;





  {:
  Defines the notifications that the protocol driver can register.
  @value ntePortOpen Notifies the protocol driver when the communication port was open.
  @value ntePortClose Notifies the protocol driver when the communication port was closed.
  @value ntePortDisconnected Notifies the protocol driver when the communication port was disconnected.
  @seealso(IPortDriverEventNotification)
  }
  TPortEvents = (ntePortOpen, ntePortClosed);


  {:
  Defines the set of notifications that a protocol driver can register.
  @seealso(TPortEvents)
  }
  TNotifyThisEvents = set of TPortEvents;

  {:
    Event notification interface for protocol drivers.
  }
  IPortDriverEventNotification = interface(IInterface)
  ['{26B0F551-5B46-49D9-BCA1-AD621B3775CF}']
    //: Returns the event to be called when communication port opens.
    function  GetPortOpenedEvent:TNotifyEvent;
    //: Returns the event to be called when communication port closed.
    function  GetPortClosedEvent:TNotifyEvent;
    {:
    Set of events that the protocol driver wants be notified.
    @seealso(TPortEvents)
    @seealso(TNotifyThisEvents)
    }
    function  NotifyThisEvents:TNotifyThisEvents;
    //: Procedure called when the communication port opens.
    procedure DoPortOpened(Sender: TObject);
    //: Procedure called when the communication port was closed.
    procedure DoPortClosed(Sender: TObject);
    //: Procedure called when the communication port was disconnected.
    procedure DoPortRemoved(Sender:TObject);
  end;

  IPortDriverEventNotificationArray = array of IPortDriverEventNotification;

  //: Communication error messsage (read or write);
  const PSM_COMMERROR        = 4;

  //: Message of communication port open, closed or disconnected.
  const PSM_PORT_EVENT       = 5;

  C_PortIOResultStr: array[TIOResult] of String = ('iorOK ',
                                                   'iorTimeOut ',
                                                   'iorNotReady ',
                                                   'iorNone ',
                                                   'iorPortError ');
implementation


end.