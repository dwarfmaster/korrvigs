package main

import (
	"fmt"
	"github.com/charmbracelet/bubbles/key"
	"github.com/charmbracelet/bubbles/list"
	tea "github.com/charmbracelet/bubbletea"
	"strconv"
)

type Item struct {
	title       string
	description string
	mk          func(model) Model
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

func newItemDelegate(mod model) list.DefaultDelegate {
	d := list.NewDefaultDelegate()
	sel := key.NewBinding(
		key.WithKeys("enter"),
		key.WithHelp("enter", "choose"),
	)

	d.UpdateFunc = func(msg tea.Msg, m *list.Model) tea.Cmd {
		switch msg := msg.(type) {
		case tea.KeyMsg:
			switch {
			case key.Matches(msg, sel):
				if i, ok := m.SelectedItem().(Item); ok {
					nmod := i.mk(mod)
					mod.stack = append(mod.stack, nmod)
				}
				return m.NewStatusMessage("Stack size: " + strconv.Itoa(len(mod.stack)))
			}
		}
		return nil
	}

	help := []key.Binding{sel}
	d.ShortHelpFunc = func() []key.Binding {
		return help
	}
	d.FullHelpFunc = func() [][]key.Binding {
		return [][]key.Binding{help}
	}
	return d
}

func makeSelector(m model) Model {
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
	delegate := newItemDelegate(m)
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
