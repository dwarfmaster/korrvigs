package main

import (
	"github.com/charmbracelet/bubbles/key"
	tea "github.com/charmbracelet/bubbletea"
)

type Model interface {
	Update(model, tea.Msg) (Model, tea.Cmd)
	View() string
}

type model struct {
	stack []Model
}

func initialModel() model {
	m := model{
		stack: []Model{},
	}
	m.stack = append(m.stack, makeSelector(m))
	return m
}

func (m model) Init() tea.Cmd {
	return nil
}

type KeyMap struct {
	Back key.Binding
	Quit key.Binding
}

var DefaultKeyMap = KeyMap{
	Back: key.NewBinding(
		key.WithKeys("q", "Backspace"),
		key.WithHelp("q/[←]", "Previous"),
	),
	Quit: key.NewBinding(
		key.WithKeys("ctrl+c"),
		key.WithHelp("C-c", "Quit"),
	),
}

func (m model) Update(msg tea.Msg) (tea.Model, tea.Cmd) {
	if len(m.stack) == 0 {
		return m, tea.Quit
	}

	var cmds []tea.Cmd
	id := len(m.stack) - 1
	model, cmd := m.stack[id].Update(m, msg)
	m.stack[id] = model
	cmds = append(cmds, cmd)

	switch msg := msg.(type) {
	case tea.KeyMsg:
		switch {
		case key.Matches(msg, DefaultKeyMap.Quit):
			cmds = append(cmds, tea.Quit)
		case key.Matches(msg, DefaultKeyMap.Back):
			m.stack = m.stack[:len(m.stack)-1]
			if len(m.stack) == 0 {
				cmds = append(cmds, tea.Quit)
			}
		}
	}
	return m, tea.Batch(cmds...)
}

func (m model) View() string {
	if len(m.stack) > 0 {
		return m.stack[len(m.stack)-1].View()
	}
	return "Shouldn't happen!"
}
