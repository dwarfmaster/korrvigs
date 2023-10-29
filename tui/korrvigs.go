package main

import (
	"fmt"
	uuid "github.com/google/uuid"
)

type DatalogValue interface {
	Display() string
}

type DatalogNumber struct {
	Value float64
}

func (n DatalogNumber) Display() string {
	return fmt.Sprintf("%f", n.Value)
}

type DatalogString struct {
	Value string
}

func (s DatalogString) Display() string {
	return fmt.Sprintf("\"%s\"", s.Value)
}

type DatalogEntry struct {
	Uuid  uuid.UUID
	sub   string
	query string
}

func (e DatalogEntry) Display() string {
	sub := "null"
	if len(e.sub) > 0 {
		sub = fmt.Sprintf("\"%s\"", e.sub)
	}
	query := "null"
	if len(e.query) > 0 {
		query = fmt.Sprintf("\"%s\"", e.query)
	}
	return fmt.Sprintf("{\"uuid\":\"%s\", \"sub\":%s, query:%s}", e.Uuid.String(), sub, query)
}

func runQuery(query string) [][]DatalogValue {
	return [][]DatalogValue{}
}
