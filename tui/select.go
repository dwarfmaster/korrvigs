package main

import (
	"fmt"
	// "github.com/charmbracelet/bubbles/key"
	"github.com/charmbracelet/bubbles/list"
	tea "github.com/charmbracelet/bubbletea"
)

type Item struct {
	title       string
	description string
	mk          func() Model
}

func (i Item) Title() string {
	return i.title
}
func (i Item) Description() string {
	return i.description
}
func (i Item) FilterValue() string {
	return fmt.Sprintf("%s %s", i.title, i.description)
}

type selector struct {
	kinds []Item
	list  list.Model
}

func makeSelector() Model {
	kinds := []Item{
		{
			title:       "Select action",
			description: "Open an interactive list with all available actions",
			mk:          makeSelector,
		},
	}

	var lst []list.Item
	for _, x := range kinds {
		lst = append(lst, x)
	}
	delegate := list.NewDefaultDelegate()
	actions := list.New(lst, delegate, 0, 0)
	actions.Title = "Actions"

	return selector{
		kinds: kinds,
		list:  actions,
	}
}

func (s selector) Update(m model, msg tea.Msg) (Model, tea.Cmd) {
	var cmds []tea.Cmd
	switch msg := msg.(type) {
	case tea.WindowSizeMsg:
		s.list.SetSize(msg.Width, msg.Height)
	}

	newListModel, cmd := s.list.Update(msg)
	s.list = newListModel
	cmds = append(cmds, cmd)

	return s, tea.Batch(cmds...)
}

func (s selector) View() string {
	return s.list.View()
}
