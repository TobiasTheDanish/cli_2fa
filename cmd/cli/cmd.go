package main

import (
	"time"

	tea "github.com/charmbracelet/bubbletea"
	"github.com/tobiasthedanish/cli_2fa/otp"
)

type otpModel struct {
	name string
	o    otp.OTP
}

var (
	mockOTPs = []struct {
		name string
		key  string
	}{
		{
			name: "RFC Test key",
			key:  "GEZDGNBVGY3TQOJQGEZDGNBVGY3TQOJQ",
		},
		{
			name: "Random key from internet",
			key:  "JBSWY3DPEHPK3PXP",
		},
	}
)

func initializer(m model) tea.Cmd {
	return func() tea.Msg {
		otps := make([]otpModel, len(mockOTPs), len(mockOTPs))
		for i, mock := range mockOTPs {
			otps[i] = otpModel{
				name: mock.name,
				o:    otp.New(mock.key).WithStep(int(m.step)),
			}
		}

		return initializerMsg{otps}
	}
}

type initializerMsg struct {
	otps []otpModel
}

func ticker(d time.Duration) tea.Cmd {
	return func() tea.Msg {
		ticker := time.NewTicker(d)
		defer ticker.Stop()

		for {
			select {
			case t := <-ticker.C:
				return tickMsg{
					t: t,
				}
			}
		}

	}
}

type tickMsg struct {
	t time.Time
}
