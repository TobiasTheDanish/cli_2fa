package main

import (
	"fmt"
	"time"

	tea "github.com/charmbracelet/bubbletea"
)

func StartTui() error {
	p := tea.NewProgram(model{step: 30})
	_, err := p.Run()
	return err
}

var (
	m         tea.Model     = model{}
	frameRate time.Duration = 66 * time.Millisecond // roughly 15 fps
)

type model struct {
	secToNext int64
	step      int64
	otps      []otpModel

	cursor int
}

func (m model) Init() tea.Cmd {
	if m.step == 0 {
		m.step = 30
	}

	t := time.Now().UTC().Unix()
	m.secToNext = 30 - (t % m.step)

	return initializer(m)
}

func (m model) Update(msg tea.Msg) (tea.Model, tea.Cmd) {
	switch msg := msg.(type) {
	case tea.KeyMsg:
		switch msg.String() {
		case "ctrl+c", "q":
			return m, tea.Quit

		case "up", "k":
			if m.cursor > 0 {
				m.cursor--
			}

		case "down", "j":
			if m.cursor < len(m.otps)-1 {
				m.cursor++
			}
		}

	case tickMsg:
		{
			t := msg.t.UTC().Unix()
			m.secToNext = 30 - (t % m.step)
			return m, ticker(frameRate)
		}

	case initializerMsg:
		{
			m.otps = msg.otps
			return m, ticker(frameRate)
		}

	}

	return m, nil
}

func (m model) View() string {
	s := fmt.Sprintf("%2d seconds until next step\n\n", m.secToNext)

	for i, om := range m.otps {
		cursor := " "
		if m.cursor == i {
			cursor = ">"
		}

		totp := om.o.Current()

		s += fmt.Sprintf(" %s %s %s\n", cursor, totp, om.name)
	}

	s += "\nPress 'Q' to quit"

	return "\n" + s + "\n\n"
}
