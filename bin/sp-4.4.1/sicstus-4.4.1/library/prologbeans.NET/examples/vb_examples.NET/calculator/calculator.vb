Option Strict Off
Option Explicit On
Imports se.sics.prologbeans
Friend Class Calculate
	Inherits System.Windows.Forms.Form
#Region "Windows Form Designer generated code "
    Public Sub New()
        MyBase.New()
        session = New PrologSession
        If m_vb6FormDefInstance Is Nothing Then
            If m_InitializingDefInstance Then
                m_vb6FormDefInstance = Me
            Else
                Try
                    'For the start-up form, the first instance created is the default instance.
                    If System.Reflection.Assembly.GetExecutingAssembly.EntryPoint.DeclaringType Is Me.GetType Then
                        m_vb6FormDefInstance = Me
                    End If
                Catch
                End Try
            End If
        End If
        'This call is required by the Windows Form Designer.
        InitializeComponent()
    End Sub
    'Form overrides dispose to clean up the component list.
    Protected Overloads Overrides Sub Dispose(ByVal Disposing As Boolean)
        If Disposing Then
            If Not components Is Nothing Then
                components.Dispose()
            End If
        End If
        MyBase.Dispose(Disposing)
    End Sub
    'Required by the Windows Form Designer
    Private components As System.ComponentModel.IContainer
    Public ToolTip1 As System.Windows.Forms.ToolTip
    Public WithEvents cmdCalc As System.Windows.Forms.Button
    Public WithEvents txtValue As System.Windows.Forms.TextBox
    Public WithEvents txtExpr As System.Windows.Forms.TextBox
    Public WithEvents cmdQuit As System.Windows.Forms.Button
    Public WithEvents Label2 As System.Windows.Forms.Label
    Public WithEvents Label1 As System.Windows.Forms.Label
    'NOTE: The following procedure is required by the Windows Form Designer
    'It can be modified using the Windows Form Designer.
    'Do not modify it using the code editor.
    <System.Diagnostics.DebuggerStepThrough()> Private Sub InitializeComponent()
        Dim resources As System.Resources.ResourceManager = New System.Resources.ResourceManager(GetType(Calculate))
        Me.components = New System.ComponentModel.Container
        Me.ToolTip1 = New System.Windows.Forms.ToolTip(components)
        Me.ToolTip1.Active = True
        Me.cmdCalc = New System.Windows.Forms.Button
        Me.txtValue = New System.Windows.Forms.TextBox
        Me.txtExpr = New System.Windows.Forms.TextBox
        Me.cmdQuit = New System.Windows.Forms.Button
        Me.Label2 = New System.Windows.Forms.Label
        Me.Label1 = New System.Windows.Forms.Label
        Me.StartPosition = System.Windows.Forms.FormStartPosition.Manual
        Me.Text = "Calculate arithmetic expressions"
        Me.ClientSize = New System.Drawing.Size(411, 117)
        Me.Location = New System.Drawing.Point(90, 180)
        Me.Font = New System.Drawing.Font("Arial", 12.0!, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, CType(238, Byte))
        Me.AutoScaleBaseSize = New System.Drawing.Size(8, 19)
        Me.BackColor = System.Drawing.SystemColors.Control
        Me.FormBorderStyle = System.Windows.Forms.FormBorderStyle.Sizable
        Me.ControlBox = True
        Me.Enabled = True
        Me.KeyPreview = False
        Me.MaximizeBox = True
        Me.MinimizeBox = True
        Me.Cursor = System.Windows.Forms.Cursors.Default
        Me.RightToLeft = System.Windows.Forms.RightToLeft.No
        Me.ShowInTaskbar = True
        Me.HelpButton = False
        Me.WindowState = System.Windows.Forms.FormWindowState.Normal
        Me.Name = "Calculate"
        Me.cmdCalc.TextAlign = System.Drawing.ContentAlignment.MiddleCenter
        Me.cmdCalc.Text = "Calculate"
        Me.cmdCalc.Font = New System.Drawing.Font("Arial", 9.75!, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, CType(0, Byte))
        Me.cmdCalc.Size = New System.Drawing.Size(92, 27)
        Me.cmdCalc.Location = New System.Drawing.Point(221, 78)
        Me.cmdCalc.TabIndex = 5
        Me.cmdCalc.BackColor = System.Drawing.SystemColors.Control
        Me.cmdCalc.CausesValidation = True
        Me.cmdCalc.Enabled = True
        Me.cmdCalc.ForeColor = System.Drawing.SystemColors.ControlText
        Me.cmdCalc.Cursor = System.Windows.Forms.Cursors.Default
        Me.cmdCalc.RightToLeft = System.Windows.Forms.RightToLeft.No
        Me.cmdCalc.TabStop = True
        Me.cmdCalc.Name = "cmdCalc"
        Me.txtValue.AutoSize = False
        Me.txtValue.Font = New System.Drawing.Font("Arial", 9.75!, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, CType(0, Byte))
        Me.txtValue.Size = New System.Drawing.Size(131, 26)
        Me.txtValue.Location = New System.Drawing.Point(260, 39)
        Me.txtValue.TabIndex = 2
        Me.txtValue.AcceptsReturn = True
        Me.txtValue.TextAlign = System.Windows.Forms.HorizontalAlignment.Left
        Me.txtValue.BackColor = System.Drawing.SystemColors.Window
        Me.txtValue.CausesValidation = True
        Me.txtValue.Enabled = True
        Me.txtValue.ForeColor = System.Drawing.SystemColors.WindowText
        Me.txtValue.HideSelection = True
        Me.txtValue.ReadOnly = False
        Me.txtValue.MaxLength = 0
        Me.txtValue.Cursor = System.Windows.Forms.Cursors.IBeam
        Me.txtValue.Multiline = False
        Me.txtValue.RightToLeft = System.Windows.Forms.RightToLeft.No
        Me.txtValue.ScrollBars = System.Windows.Forms.ScrollBars.None
        Me.txtValue.TabStop = True
        Me.txtValue.Visible = True
        Me.txtValue.BorderStyle = System.Windows.Forms.BorderStyle.Fixed3D
        Me.txtValue.Name = "txtValue"
        Me.txtExpr.AutoSize = False
        Me.txtExpr.Font = New System.Drawing.Font("Arial", 9.75!, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, CType(0, Byte))
        Me.txtExpr.Size = New System.Drawing.Size(219, 26)
        Me.txtExpr.Location = New System.Drawing.Point(16, 40)
        Me.txtExpr.TabIndex = 1
        Me.txtExpr.AcceptsReturn = True
        Me.txtExpr.TextAlign = System.Windows.Forms.HorizontalAlignment.Left
        Me.txtExpr.BackColor = System.Drawing.SystemColors.Window
        Me.txtExpr.CausesValidation = True
        Me.txtExpr.Enabled = True
        Me.txtExpr.ForeColor = System.Drawing.SystemColors.WindowText
        Me.txtExpr.HideSelection = True
        Me.txtExpr.ReadOnly = False
        Me.txtExpr.MaxLength = 0
        Me.txtExpr.Cursor = System.Windows.Forms.Cursors.IBeam
        Me.txtExpr.Multiline = False
        Me.txtExpr.RightToLeft = System.Windows.Forms.RightToLeft.No
        Me.txtExpr.ScrollBars = System.Windows.Forms.ScrollBars.None
        Me.txtExpr.TabStop = True
        Me.txtExpr.Visible = True
        Me.txtExpr.BorderStyle = System.Windows.Forms.BorderStyle.Fixed3D
        Me.txtExpr.Name = "txtExpr"
        Me.cmdQuit.TextAlign = System.Drawing.ContentAlignment.MiddleCenter
        Me.cmdQuit.Text = "Quit"
        Me.cmdQuit.Font = New System.Drawing.Font("Arial", 9.75!, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, CType(0, Byte))
        Me.cmdQuit.Size = New System.Drawing.Size(66, 28)
        Me.cmdQuit.Location = New System.Drawing.Point(325, 78)
        Me.cmdQuit.TabIndex = 0
        Me.cmdQuit.BackColor = System.Drawing.SystemColors.Control
        Me.cmdQuit.CausesValidation = True
        Me.cmdQuit.Enabled = True
        Me.cmdQuit.ForeColor = System.Drawing.SystemColors.ControlText
        Me.cmdQuit.Cursor = System.Windows.Forms.Cursors.Default
        Me.cmdQuit.RightToLeft = System.Windows.Forms.RightToLeft.No
        Me.cmdQuit.TabStop = True
        Me.cmdQuit.Name = "cmdQuit"
        Me.Label2.Text = "Value:"
        Me.Label2.Font = New System.Drawing.Font("Arial", 9.75!, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, CType(0, Byte))
        Me.Label2.Size = New System.Drawing.Size(113, 17)
        Me.Label2.Location = New System.Drawing.Point(260, 13)
        Me.Label2.TabIndex = 4
        Me.Label2.TextAlign = System.Drawing.ContentAlignment.TopLeft
        Me.Label2.BackColor = System.Drawing.SystemColors.Control
        Me.Label2.Enabled = True
        Me.Label2.ForeColor = System.Drawing.SystemColors.ControlText
        Me.Label2.Cursor = System.Windows.Forms.Cursors.Default
        Me.Label2.RightToLeft = System.Windows.Forms.RightToLeft.No
        Me.Label2.UseMnemonic = True
        Me.Label2.Visible = True
        Me.Label2.AutoSize = False
        Me.Label2.BorderStyle = System.Windows.Forms.BorderStyle.None
        Me.Label2.Name = "Label2"
        Me.Label1.Text = "Enter expression:"
        Me.Label1.Font = New System.Drawing.Font("Arial", 9.75!, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, CType(0, Byte))
        Me.Label1.Size = New System.Drawing.Size(206, 17)
        Me.Label1.Location = New System.Drawing.Point(16, 13)
        Me.Label1.TabIndex = 3
        Me.Label1.TextAlign = System.Drawing.ContentAlignment.TopLeft
        Me.Label1.BackColor = System.Drawing.SystemColors.Control
        Me.Label1.Enabled = True
        Me.Label1.ForeColor = System.Drawing.SystemColors.ControlText
        Me.Label1.Cursor = System.Windows.Forms.Cursors.Default
        Me.Label1.RightToLeft = System.Windows.Forms.RightToLeft.No
        Me.Label1.UseMnemonic = True
        Me.Label1.Visible = True
        Me.Label1.AutoSize = False
        Me.Label1.BorderStyle = System.Windows.Forms.BorderStyle.None
        Me.Label1.Name = "Label1"
        Me.Controls.Add(cmdCalc)
        Me.Controls.Add(txtValue)
        Me.Controls.Add(txtExpr)
        Me.Controls.Add(cmdQuit)
        Me.Controls.Add(Label2)
        Me.Controls.Add(Label1)
    End Sub
#End Region
#Region "Upgrade Support "
    Private Shared m_vb6FormDefInstance As calculate
    Private Shared m_InitializingDefInstance As Boolean
    Public Shared Property DefInstance() As calculate
        Get
            If m_vb6FormDefInstance Is Nothing OrElse m_vb6FormDefInstance.IsDisposed Then
                m_InitializingDefInstance = True
                m_vb6FormDefInstance = New Calculate
                m_InitializingDefInstance = False
            End If
            DefInstance = m_vb6FormDefInstance
        End Get
        Set(ByVal Value As calculate)
            m_vb6FormDefInstance = Value
        End Set
    End Property
#End Region

    Private session As PrologSession

    Private Sub cmdQuit_Click(ByVal eventSender As System.Object, ByVal eventArgs As System.EventArgs) Handles cmdQuit.Click
        Me.Close()
    End Sub

    Public Function calculate(ByVal Expr As String) As String
        Dim bindings as Bindings
        Dim answer as QueryAnswer
        Dim result as PBTerm

'        bindings = New Bindings().bind("E", textBox1.Text + '.')
        bindings = New Bindings().bind("E", txtExpr.Text & ".")
        session.connect()	' This will connect if neccessary.
        answer = session.executeQuery("evaluate(E,R)", bindings)
        result = answer.getValue("R")
        If result Is Nothing Then
	      calculate = "Error: " & answer.getError
        Else
	      calculate = txtExpr.Text & " = " & result.toString
	      txtExpr.Clear()
        End If
                
        Exit Function

Err_Renamed:
        MsgBox("Bad expression", 48, "Error!")
        calculate = ""
    End Function

    Private Sub cmdCalc_Click(ByVal eventSender As System.Object, ByVal eventArgs As System.EventArgs) Handles cmdCalc.Click
        txtValue.Text = calculate(txtExpr.Text)
    End Sub

    Private Sub Calculate_Load(ByVal eventSender As System.Object, ByVal eventArgs As System.EventArgs) Handles MyBase.Load
        '   If PrologInit() <> 1 Then GoTo Err_Renamed
        '   If PrologQueryCutFail("ensure_loaded(app(calc))") <> 1 Then GoTo Err_Renamed
        Exit Sub

Err_Renamed:
        MsgBox("Prolog initialization failed", 48, "Error")
        Me.Close()
    End Sub

    'UPGRADE_WARNING: Form event Calculate.Unload has a new behavior. Click for more: 'ms-help://MS.VSCC.2003/commoner/redir/redirect.htm?keyword="vbup2065"'
    Private Sub Calculate_Closed(ByVal eventSender As System.Object, ByVal eventArgs As System.EventArgs) Handles MyBase.Closed
        '   PrologDeInit()
    End Sub

End Class
