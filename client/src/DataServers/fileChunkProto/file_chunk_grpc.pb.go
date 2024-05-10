// Code generated by protoc-gen-go-grpc. DO NOT EDIT.
// versions:
// - protoc-gen-go-grpc v1.2.0
// - protoc             v3.12.4
// source: file_chunk.proto

package fileChunkProto

import (
	context "context"
	grpc "google.golang.org/grpc"
	codes "google.golang.org/grpc/codes"
	status "google.golang.org/grpc/status"
)

// This is a compile-time assertion to ensure that this generated file
// is compatible with the grpc package it is being compiled against.
// Requires gRPC-Go v1.32.0 or later.
const _ = grpc.SupportPackageIsVersion7

// FileClient is the client API for File service.
//
// For semantics around ctx use and closing/ending streaming RPCs, please refer to https://pkg.go.dev/google.golang.org/grpc/?tab=doc#ClientConn.NewStream.
type FileClient interface {
	Download(ctx context.Context, in *DownloadMessage, opts ...grpc.CallOption) (File_DownloadClient, error)
	Upload(ctx context.Context, opts ...grpc.CallOption) (File_UploadClient, error)
}

type fileClient struct {
	cc grpc.ClientConnInterface
}

func NewFileClient(cc grpc.ClientConnInterface) FileClient {
	return &fileClient{cc}
}

func (c *fileClient) Download(ctx context.Context, in *DownloadMessage, opts ...grpc.CallOption) (File_DownloadClient, error) {
	stream, err := c.cc.NewStream(ctx, &File_ServiceDesc.Streams[0], "/File/Download", opts...)
	if err != nil {
		return nil, err
	}
	x := &fileDownloadClient{stream}
	if err := x.ClientStream.SendMsg(in); err != nil {
		return nil, err
	}
	if err := x.ClientStream.CloseSend(); err != nil {
		return nil, err
	}
	return x, nil
}

type File_DownloadClient interface {
	Recv() (*FileMessage, error)
	grpc.ClientStream
}

type fileDownloadClient struct {
	grpc.ClientStream
}

func (x *fileDownloadClient) Recv() (*FileMessage, error) {
	m := new(FileMessage)
	if err := x.ClientStream.RecvMsg(m); err != nil {
		return nil, err
	}
	return m, nil
}

func (c *fileClient) Upload(ctx context.Context, opts ...grpc.CallOption) (File_UploadClient, error) {
	stream, err := c.cc.NewStream(ctx, &File_ServiceDesc.Streams[1], "/File/Upload", opts...)
	if err != nil {
		return nil, err
	}
	x := &fileUploadClient{stream}
	return x, nil
}

type File_UploadClient interface {
	Send(*FileMessage) error
	CloseAndRecv() (*UploadMessage, error)
	grpc.ClientStream
}

type fileUploadClient struct {
	grpc.ClientStream
}

func (x *fileUploadClient) Send(m *FileMessage) error {
	return x.ClientStream.SendMsg(m)
}

func (x *fileUploadClient) CloseAndRecv() (*UploadMessage, error) {
	if err := x.ClientStream.CloseSend(); err != nil {
		return nil, err
	}
	m := new(UploadMessage)
	if err := x.ClientStream.RecvMsg(m); err != nil {
		return nil, err
	}
	return m, nil
}

// FileServer is the server API for File service.
// All implementations must embed UnimplementedFileServer
// for forward compatibility
type FileServer interface {
	Download(*DownloadMessage, File_DownloadServer) error
	Upload(File_UploadServer) error
	mustEmbedUnimplementedFileServer()
}

// UnimplementedFileServer must be embedded to have forward compatible implementations.
type UnimplementedFileServer struct {
}

func (UnimplementedFileServer) Download(*DownloadMessage, File_DownloadServer) error {
	return status.Errorf(codes.Unimplemented, "method Download not implemented")
}
func (UnimplementedFileServer) Upload(File_UploadServer) error {
	return status.Errorf(codes.Unimplemented, "method Upload not implemented")
}
func (UnimplementedFileServer) mustEmbedUnimplementedFileServer() {}

// UnsafeFileServer may be embedded to opt out of forward compatibility for this service.
// Use of this interface is not recommended, as added methods to FileServer will
// result in compilation errors.
type UnsafeFileServer interface {
	mustEmbedUnimplementedFileServer()
}

func RegisterFileServer(s grpc.ServiceRegistrar, srv FileServer) {
	s.RegisterService(&File_ServiceDesc, srv)
}

func _File_Download_Handler(srv interface{}, stream grpc.ServerStream) error {
	m := new(DownloadMessage)
	if err := stream.RecvMsg(m); err != nil {
		return err
	}
	return srv.(FileServer).Download(m, &fileDownloadServer{stream})
}

type File_DownloadServer interface {
	Send(*FileMessage) error
	grpc.ServerStream
}

type fileDownloadServer struct {
	grpc.ServerStream
}

func (x *fileDownloadServer) Send(m *FileMessage) error {
	return x.ServerStream.SendMsg(m)
}

func _File_Upload_Handler(srv interface{}, stream grpc.ServerStream) error {
	return srv.(FileServer).Upload(&fileUploadServer{stream})
}

type File_UploadServer interface {
	SendAndClose(*UploadMessage) error
	Recv() (*FileMessage, error)
	grpc.ServerStream
}

type fileUploadServer struct {
	grpc.ServerStream
}

func (x *fileUploadServer) SendAndClose(m *UploadMessage) error {
	return x.ServerStream.SendMsg(m)
}

func (x *fileUploadServer) Recv() (*FileMessage, error) {
	m := new(FileMessage)
	if err := x.ServerStream.RecvMsg(m); err != nil {
		return nil, err
	}
	return m, nil
}

// File_ServiceDesc is the grpc.ServiceDesc for File service.
// It's only intended for direct use with grpc.RegisterService,
// and not to be introspected or modified (even as a copy)
var File_ServiceDesc = grpc.ServiceDesc{
	ServiceName: "File",
	HandlerType: (*FileServer)(nil),
	Methods:     []grpc.MethodDesc{},
	Streams: []grpc.StreamDesc{
		{
			StreamName:    "Download",
			Handler:       _File_Download_Handler,
			ServerStreams: true,
		},
		{
			StreamName:    "Upload",
			Handler:       _File_Upload_Handler,
			ClientStreams: true,
		},
	},
	Metadata: "file_chunk.proto",
}
